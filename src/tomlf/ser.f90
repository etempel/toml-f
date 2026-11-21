! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML serialization module
!>
!> This module provides interfaces for serializing TOML data structures
!> back to TOML format. The primary interfaces are:
!>
!> - [[toml_dump]]: Write a TOML table to a file or I/O unit
!> - [[toml_dumps]]: Serialize a TOML table to a string
!> - [[toml_serialize]]: Low-level serialization using the visitor pattern
!>
!> The [[toml_serializer]] type implements the visitor pattern and can be
!> used directly for custom serialization workflows.
!>
!> A TOML document is UTF-8. A key or string that is not valid UTF-8 is refused:
!> [[toml_dumps]] and [[toml_dump]] return an error with `stat` set to
!> `toml_stat%invalid_utf8`, naming the key's dotted path, and write nothing.
module tomlf_ser
   use tomlf_constants, only : tfc, tfi, tfr, tfout, toml_type
   use tomlf_datetime, only : toml_datetime, to_string
   use tomlf_error, only : toml_error, toml_stat, make_error
   use tomlf_type, only : toml_value, toml_visitor, toml_key, toml_table, &
      & toml_array, toml_keyval, is_array_of_tables, len
   use tomlf_utils, only : to_string, toml_escape_string, valid_utf8, show_invalid_utf8
   implicit none
   private

   public :: toml_serializer, new_serializer, new
   public :: toml_dump, toml_dumps, toml_serialize


   interface toml_dumps
      module procedure :: toml_dump_to_string
   end interface toml_dumps

   interface toml_dump
      module procedure :: toml_dump_to_file
      module procedure :: toml_dump_to_unit
   end interface toml_dump


   !> Configuration for JSON serializer
   type :: toml_ser_config

      !> Indentation
      character(len=:), allocatable :: indent

   end type toml_ser_config


   !> TOML serializer to produduce a TOML document from a datastructure
   type, extends(toml_visitor) :: toml_serializer
      private

      !> Output buffer, of which the first length characters are written
      character(:), allocatable :: output

      !> Number of characters written to the output buffer
      integer :: length = 0

      !> Configuration for serializer
      type(toml_ser_config) :: config

      !> Special mode for printing array of tables
      logical, private :: array_of_tables

      !> Special mode for printing inline arrays
      logical, private :: inline_array

      !> Top of the key stack
      integer, private :: top

      !> Key stack to create table headers, and to name the place of an error
      type(toml_key), allocatable, private :: stack(:)

      !> First error met while rendering, with stat toml_stat%invalid_utf8 for a key or
      !> string that is not valid UTF-8; the text rendered is then not TOML
      type(toml_error), allocatable, public :: error

   contains

      !> Visit a TOML value
      procedure :: visit

   end type toml_serializer


   !> Create standard constructor
   interface toml_serializer
      module procedure :: new_serializer_func
   end interface toml_serializer


   !> Overloaded constructor for TOML serializers
   interface new
      module procedure :: new_serializer
   end interface


   !> Initial size of the key path stack
   integer, parameter :: initial_size = 8


contains


!> Serialize a JSON value to a string and return it.
!>
!> In case of an error this function will invoke an error stop.
function toml_serialize(val, config) result(string)
   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   !> Serialized JSON value
   character(len=:), allocatable :: string

   type(toml_error), allocatable :: error

   call toml_dumps(val, string, error, config=config)
   if (allocated(error)) then
      print "(a)", "Error: " // error%message
      error stop 1
   end if
end function toml_serialize


!> Create a string representing the TOML value.
!>
!> A key or string that is not valid UTF-8 gives an error with stat `toml_stat%invalid_utf8`,
!> naming the key's dotted path, and an empty string.
subroutine toml_dump_to_string(val, string, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Formatted unit to write to
   character(:), allocatable, intent(out) :: string

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   type(toml_serializer) :: ser

   ser = toml_serializer(config=config)
   call val%accept(ser)
   if (allocated(ser%error)) then
      call move_alloc(ser%error, error)
      string = ""
      return
   end if
   string = ser%output(:ser%length)
end subroutine toml_dump_to_string


!> Write string representation of JSON value to a connected formatted unit
subroutine toml_dump_to_unit(val, io, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Formatted unit to write to
   integer, intent(in) :: io

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   character(len=:), allocatable :: string
   character(512) :: msg
   integer :: stat

   call toml_dumps(val, string, error, config=config)
   if (allocated(error)) return
   write(io, "(a)", iostat=stat, iomsg=msg) string
   if (stat /= 0) then
      call make_error(error, trim(msg))
      return
   end if
end subroutine toml_dump_to_unit


!> Write string representation of TOML value to a file.
!>
!> The value is rendered before the file is opened: on an error in rendering, such as a key
!> or string that is not valid UTF-8, an existing file is left as it was and no new one is
!> created.
subroutine toml_dump_to_file(val, filename, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> File name to write to
   character(*), intent(in) :: filename

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   character(len=:), allocatable :: string
   integer :: io
   integer :: stat
   character(512) :: msg

   ! Render before touching the file: a document that cannot be rendered leaves an existing
   ! file as it was and creates no new one
   call toml_dumps(val, string, error, config=config)
   if (allocated(error)) return

   open(file=filename, newunit=io, iostat=stat, iomsg=msg)
   if (stat /= 0) then
      call make_error(error, trim(msg))
      return
   end if
   write(io, "(a)", iostat=stat, iomsg=msg) string
   if (stat /= 0) then
      call make_error(error, trim(msg))
      close(unit=io, iostat=stat)
      return
   end if
   close(unit=io, iostat=stat, iomsg=msg)
   if (stat /= 0) then
      call make_error(error, trim(msg))
   end if
end subroutine toml_dump_to_file


!> Constructor to create new serializer instance
subroutine new_serializer(self, config)

   !> Instance of the TOML serializer
   type(toml_serializer), intent(out) :: self

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   self%output = ""
   self%length = 0
   self%array_of_tables = .false.
   self%inline_array = .false.
   self%top = 0
   self%config = toml_ser_config()
   if (present(config)) self%config = config
end subroutine new_serializer


!> Default constructor for TOML serializer
function new_serializer_func(config) result(self)

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   !> Instance of the TOML serializer
   type(toml_serializer) :: self

   call new_serializer(self, config)
end function new_serializer_func


!> Visit a TOML value
recursive subroutine visit(self, val)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: self

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   ! After the first error the output is discarded: render nothing more
   if (allocated(self%error)) return

   select type(val)
   class is(toml_keyval)
      call visit_keyval(self, val)
   class is(toml_array)
      call visit_array(self, val)
   class is(toml_table)
      call visit_table(self, val)
   end select

end subroutine visit


!> Visit a TOML key-value pair
subroutine visit_keyval(visitor, keyval)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_keyval), intent(inout) :: keyval

   character(kind=tfc, len=:), allocatable :: key, str
   type(toml_datetime), pointer :: dval
   character(:, tfc), pointer :: sval
   integer(tfi), pointer :: ival
   real(tfr), pointer :: rval
   logical, pointer :: lval

   call check_key(visitor, keyval)
   call keyval%get_key(key)

   select case(keyval%get_type())
   case(toml_type%string)
      call keyval%get(sval)
      call check_utf8(visitor, sval, keyval)
      call toml_escape_string(sval, str)
   case(toml_type%int)
      call keyval%get(ival)
      str = to_string(ival)
   case(toml_type%float)
      call keyval%get(rval)
      str = to_string(rval)
   case(toml_type%boolean)
      call keyval%get(lval)
      if (lval) then
         str = "true"
      else
         str = "false"
      end if
   case(toml_type%datetime)
      call keyval%get(dval)
      str = to_string(dval)
   case default
   end select

   if (visitor%inline_array) then
      call append(visitor, " ")
   end if
   call append(visitor, key // " = " // str)
   if (.not.visitor%inline_array) then
      call append(visitor, new_line("a"))
   end if

end subroutine visit_keyval


!> Visit a TOML array
recursive subroutine visit_array(visitor, array)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_array), intent(inout) :: array

   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: key, str
   type(toml_datetime), pointer :: dval
   character(:, tfc), pointer :: sval
   integer(tfi), pointer :: ival
   real(tfr), pointer :: rval
   logical, pointer :: lval
   integer :: i, n

   if (visitor%inline_array) call append(visitor, " [")
   n = len(array)
   do i = 1, n
      call array%get(i, ptr)
      select type(ptr)
      class is(toml_keyval)

         select case(ptr%get_type())
         case(toml_type%string)
            call ptr%get(sval)
            call check_utf8(visitor, sval, ptr)
            call toml_escape_string(sval, str)
         case(toml_type%int)
            call ptr%get(ival)
            str = to_string(ival)
         case(toml_type%float)
            call ptr%get(rval)
            str = to_string(rval)
         case(toml_type%boolean)
            call ptr%get(lval)
            if (lval) then
               str = "true"
            else
               str = "false"
            end if
         case(toml_type%datetime)
            call ptr%get(dval)
            str = to_string(dval)
         case default
         end select

         call append(visitor, " " // str)
         if (i /= n) call append(visitor, ",")
      class is(toml_array)
         call ptr%accept(visitor)
         if (i /= n) call append(visitor, ",")
      class is(toml_table)
         if (visitor%inline_array) then
            call append(visitor, " {")
            call ptr%accept(visitor)
            call append(visitor, " }")
            if (i /= n) call append(visitor, ",")
         else
            visitor%array_of_tables = .true.
            call push_key(visitor, array, key)
            call ptr%accept(visitor)
            call pop_key(visitor)
         end if
      end select
   end do
   if (visitor%inline_array) call append(visitor, " ]")

end subroutine visit_array


!> Visit a TOML table
recursive subroutine visit_table(visitor, table)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML table to visit
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   type(toml_key), allocatable :: list(:)
   logical, allocatable :: defer(:)
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n

   call table%get_keys(list)

   n = size(list, 1)
   allocate(defer(n))

   if (.not.allocated(visitor%stack)) then
      call resize(visitor%stack)
   else
      if (.not.(visitor%inline_array .or. table%implicit)) then
         call append(visitor, "[")
         if (visitor%array_of_tables) call append(visitor, "[")
         do i = 1, visitor%top-1
            call append(visitor, visitor%stack(i)%key // ".")
         end do
         call append(visitor, visitor%stack(visitor%top)%key)
         call append(visitor, "]")
         if (visitor%array_of_tables) call append(visitor, "]")
         call append(visitor, new_line("a"))
         visitor%array_of_tables = .false.
      end if
   end if

   do i = 1, n
      defer(i) = .false.
      call table%get(list(i)%key, ptr)
      select type(ptr)
      class is(toml_keyval)
         call ptr%accept(visitor)
         if (visitor%inline_array) then
            if (i /= n) call append(visitor, ",")
         end if
      class is(toml_array)
         if (visitor%inline_array) then
            call push_key(visitor, ptr, key)
            call append(visitor, " " // key // " =")
            call ptr%accept(visitor)
            call pop_key(visitor)
            if (i /= n) call append(visitor, ",")
         else
            if (is_array_of_tables(ptr)) then
               ! Array of tables open a new section
               ! -> cannot serialize them before all key-value pairs are done
               defer(i) = .true.
            else
               call push_key(visitor, ptr, key)
               visitor%inline_array = .true.
               call append(visitor, key // " =")
               call ptr%accept(visitor)
               visitor%inline_array = .false.
               call pop_key(visitor)
               call append(visitor, new_line("a"))
            end if
         end if
      class is(toml_table)
         if (visitor%inline_array) then
            ! Inside an inline array every entry is written in place, in index order:
            ! a subtable as an inline table under its own key
            call push_key(visitor, ptr, key)
            call append(visitor, " " // key // " = {")
            call ptr%accept(visitor)
            call append(visitor, " }")
            call pop_key(visitor)
            if (i /= n) call append(visitor, ",")
         else
            ! Subtables open a new section
            ! -> cannot serialize them before all key-value pairs are done
            defer(i) = .true.
         end if
      end select
   end do

   do i = 1, n
      if (defer(i)) then
         call table%get(list(i)%key, ptr)
         select type(ptr)
         class is(toml_keyval)
            call ptr%accept(visitor)
            if (visitor%inline_array) then
               if (i /= n) call append(visitor, ",")
            end if
         class is(toml_array)
            if (visitor%inline_array) then
               call push_key(visitor, ptr, key)
               call append(visitor, " " // key // " =")
               call ptr%accept(visitor)
               call pop_key(visitor)
               if (i /= n) call append(visitor, ",")
            else
               if (is_array_of_tables(ptr)) then
                  call ptr%accept(visitor)
               else
                  call push_key(visitor, ptr, key)
                  visitor%inline_array = .true.
                  call append(visitor, key // " =")
                  call ptr%accept(visitor)
                  visitor%inline_array = .false.
                  call pop_key(visitor)
                  call append(visitor, new_line("a"))
               end if
            end if
         class is(toml_table)
            call push_key(visitor, ptr, key)
            call ptr%accept(visitor)
            call pop_key(visitor)
         end select
      end if
   end do

   if (.not.visitor%inline_array .and. visitor%top == 0) then
      deallocate(visitor%stack)
   end if

end subroutine visit_table


!> Put the key of a value on the key stack, after checking that it is valid UTF-8
subroutine push_key(visitor, val, key)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value whose key goes on the stack
   class(toml_value), intent(in) :: val

   !> The key as it is written, bare or quoted
   character(kind=tfc, len=:), allocatable, intent(out) :: key

   call check_key(visitor, val)
   call val%get_key(key)
   if (.not.allocated(key)) key = ""
   if (.not.allocated(visitor%stack)) call resize(visitor%stack)
   if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
   visitor%top = visitor%top + 1
   visitor%stack(visitor%top)%key = key

end subroutine push_key


!> Take the last key off the key stack
subroutine pop_key(visitor)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   deallocate(visitor%stack(visitor%top)%key)
   visitor%top = visitor%top - 1

end subroutine pop_key


!> Check that the key of a value is valid UTF-8
subroutine check_key(visitor, val)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value whose key is checked
   class(toml_value), intent(in) :: val

   if (allocated(val%key)) call check_utf8(visitor, val%key, val)

end subroutine check_key


!> Check that text, the key or a string of a value, is valid UTF-8. On the first failure,
!> and only the first, record an error naming the place: the keys on the stack, then the
!> value's own key if it has one, each byte outside a character shown as \xHH.
subroutine check_utf8(visitor, text, val)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> Raw text to check
   character(kind=tfc, len=*), intent(in) :: text

   !> TOML value the text belongs to
   class(toml_value), intent(in) :: val

   character(kind=tfc, len=:), allocatable :: path, key
   integer :: i

   if (allocated(visitor%error)) return
   if (valid_utf8(text)) return

   path = ""
   do i = 1, visitor%top
      if (i > 1) path = path // "."
      path = path // visitor%stack(i)%key
   end do
   call val%get_key(key)
   if (allocated(key)) then
      if (len(path) > 0) path = path // "."
      path = path // show_invalid_utf8(key)
   end if

   if (len(path) > 0) then
      call make_error(visitor%error, "Key or string is not valid UTF-8: " // path, &
         & toml_stat%invalid_utf8)
   else
      call make_error(visitor%error, "Key or string is not valid UTF-8", toml_stat%invalid_utf8)
   end if

end subroutine check_utf8


!> Change size of the stack
subroutine resize(stack, n)

   !> Stack of keys to be resized
   type(toml_key), allocatable, intent(inout) :: stack(:)

   !> New size of the stack
   integer, intent(in), optional :: n

   type(toml_key), allocatable :: tmp(:)
   integer :: m

   if (present(n)) then
      m = n
   else
      if (allocated(stack)) then
         m = size(stack)
         m = m + m/2 + 1
      else
         m = initial_size
      end if
   end if

   if (allocated(stack)) then
      call move_alloc(stack, tmp)
      allocate(stack(m))

      m = min(size(tmp), m)
      stack(:m) = tmp(:m)

      deallocate(tmp)
   else
      allocate(stack(m))
   end if

end subroutine resize


!> Append text to the output, doubling the buffer's capacity when it is full: growing it
!> by concatenation would copy everything written so far on every append
subroutine append(visitor, text)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> Text to append
   character(*), intent(in) :: text

   character(:), allocatable :: tmp
   integer :: capacity

   if (.not.allocated(visitor%output)) then
      allocate(character(0) :: visitor%output)
      visitor%length = 0
   end if

   capacity = len(visitor%output)
   if (visitor%length + len(text) > capacity) then
      capacity = max(2*capacity, visitor%length + len(text), 1024)
      allocate(character(capacity) :: tmp)
      tmp(:visitor%length) = visitor%output(:visitor%length)
      call move_alloc(tmp, visitor%output)
   end if

   visitor%output(visitor%length+1:visitor%length+len(text)) = text
   visitor%length = visitor%length + len(text)
end subroutine append


end module tomlf_ser

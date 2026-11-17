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

module tftest_utils
   use, intrinsic :: iso_fortran_env, only : int64
   use testdrive
   implicit none
   private

   public :: collect_utils


contains


!> Collect all exported unit tests
subroutine collect_utils(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("string-i1", string_i1), &
      & new_unittest("string-i2", string_i2), &
      & new_unittest("string-i4", string_i4), &
      & new_unittest("string-i8", string_i8), &
      & new_unittest("escape-backslash", escape_backslash), &
      & new_unittest("escape-string", escape_string), &
      & new_unittest("string-r8-subnormal", string_r8_subnormal)]

end subroutine collect_utils


subroutine string_i1(error)
   use tomlf_constants, only : tf_i1
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_string(-huge(1_tf_i1) - 1_tf_i1), "-128")
end subroutine string_i1


subroutine string_i2(error)
   use tomlf_constants, only : tf_i2
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_string(-huge(1_tf_i2) - 1_tf_i2), "-32768")
end subroutine string_i2


subroutine string_i4(error)
   use tomlf_constants, only : tf_i4
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_string(-huge(1_tf_i4) - 1_tf_i4), "-2147483648")
end subroutine string_i4


subroutine string_i8(error)
   use tomlf_constants, only : tfi
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_string(-huge(1_tfi) - 1_tfi), "-9223372036854775808")
end subroutine string_i8


subroutine escape_backslash(error)
   use tomlf_constants, only : tfc, toml_escape

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, toml_escape%backslash, achar(92, kind=tfc))
end subroutine escape_backslash


!> The escaper gives the text the earlier one-concatenation-per-character escaper gave, for
!> every string without a control character that has no short escape, and writes each of
!> those as a four-digit upper-case unicode escape
subroutine escape_string(error)
   use tomlf_constants, only : tfc
   use tomlf_utils, only : toml_escape_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(*, tfc), parameter :: bs = achar(92), hexdigit = "0123456789ABCDEF"
   integer, parameter :: lengths(4) = [10, 100, 1000, 10000]
   character(kind=tfc, len=:), allocatable :: raw, escaped
   integer, allocatable :: plain(:)
   integer :: i, j, it, code, imode, nplain
   integer(int64) :: state
   logical :: mode(2)
   character(32) :: what

   mode = [.false., .true.]

   ! The control characters that have no short escape
   do code = 0, 127
      if (.not.controls(code)) cycle
      call toml_escape_string(achar(code, kind=tfc), escaped)
      write(what, '("control character ", i0)') code
      call check(error, escaped, '"'//bs//"u00"//hexdigit(code/16+1:code/16+1)// &
         & hexdigit(mod(code, 16)+1:mod(code, 16)+1)//'"', trim(what))
      if (allocated(error)) return
   end do

   ! Every other byte may appear in a string compared with the earlier escaper
   nplain = 0
   allocate(plain(256))
   do code = 0, 255
      if (controls(code)) cycle
      nplain = nplain + 1
      plain(nplain) = code
   end do

   do imode = 1, 2
      ! All one-byte strings
      do i = 1, nplain
         raw = achar(plain(i), kind=tfc)
         call compare(error, raw, mode(imode))
         if (allocated(error)) return
      end do
      ! All two-byte strings
      do i = 1, nplain
         do j = 1, nplain
            raw = achar(plain(i), kind=tfc)//achar(plain(j), kind=tfc)
            call compare(error, raw, mode(imode))
            if (allocated(error)) return
         end do
      end do
   end do

   ! A few long strings of random bytes, from a fixed seed
   state = 20261117_int64
   do it = 1, size(lengths)
      if (allocated(raw)) deallocate(raw)
      allocate(character(kind=tfc, len=lengths(it)) :: raw)
      do i = 1, len(raw)
         state = mod(1103515245_int64*state + 12345_int64, 2147483648_int64)
         raw(i:i) = achar(plain(1 + int(mod(state / 65536_int64, int(nplain, int64)))), kind=tfc)
      end do
      do imode = 1, 2
         call compare(error, raw, mode(imode))
         if (allocated(error)) return
      end do
   end do

contains

   !> Whether code is a control character without a short escape
   pure function controls(code)
      integer, intent(in) :: code
      logical :: controls

      select case(code)
      case(0:7, 11, 14:31, 127)
         controls = .true.
      case default
         controls = .false.
      end select
   end function controls

   !> Compare the escaper with the reference for one string
   subroutine compare(error, raw, multiline)
      type(error_type), allocatable, intent(out) :: error
      character(kind=tfc, len=*), intent(in) :: raw
      logical, intent(in) :: multiline

      character(kind=tfc, len=:), allocatable :: escaped, expected
      character(64) :: what
      integer :: ib

      call toml_escape_string(raw, escaped, multiline)
      call reference_escape_string(raw, expected, multiline)
      if (len(escaped) /= len(expected) .or. escaped /= expected) then
         if (len(raw) <= 2) then
            write(what, '("bytes", 2(1x, i0))') (iachar(raw(ib:ib)), ib = 1, len(raw))
         else
            write(what, '("a random string of ", i0, " bytes")') len(raw)
         end if
         call test_failed(error, "escaped text differs from the earlier escaper for "//trim(what))
      end if
   end subroutine compare

end subroutine escape_string


!> A subnormal is not written as a zero, also under abrupt underflow (flush-to-zero and
!> denormals-are-zero), where it compares equal to zero; a zero is written with its sign.
!> Every value is made from its bits, so that no arithmetic flushes it.
subroutine string_r8_subnormal(error)
   use, intrinsic :: ieee_arithmetic, only : ieee_support_underflow_control, &
      & ieee_get_underflow_mode, ieee_set_underflow_mode
   use tomlf_constants, only : tfi, tfr
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(tfr) :: smallest, largest, zero, negzero
   character(:), allocatable :: text_smallest, text_negative, text_largest, text_abrupt, expected
   character(32) :: buffer
   logical :: gradual

   ! The smallest subnormal, the largest subnormal, +0 and -0
   smallest = transfer(1_tfi, 0.0_tfr)
   largest = transfer(huge(0_tfi) / 2_tfi**11, 0.0_tfr)
   zero = transfer(0_tfi, 0.0_tfr)
   negzero = transfer(-huge(0_tfi) - 1_tfi, 0.0_tfr)

   text_smallest = to_string(smallest)
   call check(error, index(text_smallest, "E-324") > 0 .and. text_smallest(1:1) /= "-", &
      & "the smallest subnormal must be written as itself, not "//text_smallest)
   if (allocated(error)) return
   text_negative = to_string(-smallest)
   call check(error, index(text_negative, "E-324") > 0 .and. text_negative(1:1) == "-", &
      & "the negative smallest subnormal must be written as itself, not "//text_negative)
   if (allocated(error)) return
   text_largest = to_string(largest)
   call check(error, index(text_largest, "E-308") > 0, &
      & "the largest subnormal must be written as itself, not "//text_largest)
   if (allocated(error)) return
   call check(error, to_string(zero), "0.0")
   if (allocated(error)) return
   call check(error, to_string(negzero), "-0.0")
   if (allocated(error)) return

   ! Under abrupt underflow smallest == 0 is true, and whether ES editing still writes its
   ! digits is the runtime's matter: gfortran's and nagfor's write zero digits, ifx's the
   ! value. The writer must keep the digits whenever the runtime gives them.
   if (.not.ieee_support_underflow_control(smallest)) return
   call ieee_get_underflow_mode(gradual)
   call ieee_set_underflow_mode(.false.)
   text_abrupt = to_string(smallest)
   write(buffer, '(es24.16e3)') smallest
   call ieee_set_underflow_mode(gradual)
   if (scan(buffer(:index(buffer, "E")), "123456789") > 0) then
      expected = trim(adjustl(buffer))
   else
      expected = "0.0"
   end if
   call check(error, text_abrupt, expected, &
      & "under abrupt underflow a subnormal must be written as ES editing writes it")
end subroutine string_r8_subnormal


!> The escaper of toml-f at d08aec0, which appends one character at a time, kept as a
!> reference for the text of every string it already escaped correctly
subroutine reference_escape_string(raw, escaped, multiline)
   use tomlf_constants, only : tfc, TOML_NEWLINE, TOML_FORMFEED, TOML_CARRIAGE_RETURN, &
      & TOML_TABULATOR, TOML_BACKSPACE

   !> Raw representation of TOML string
   character(kind=tfc, len=*), intent(in) :: raw
   !> Escaped view of the TOML string
   character(kind=tfc, len=:), allocatable, intent(out) :: escaped
   !> Preserve newline characters
   logical, intent(in) :: multiline

   integer :: i

   escaped = '"'
   do i = 1, len(raw)
      select case(raw(i:i))
      case default; escaped = escaped // raw(i:i)
      case("\"); escaped = escaped // "\\"
      case('"'); escaped = escaped // '\"'
      case(TOML_NEWLINE)
         if (multiline) then
            escaped = escaped // raw(i:i)
         else
            escaped = escaped // "\n"
         end if
      case(TOML_FORMFEED); escaped = escaped // "\f"
      case(TOML_CARRIAGE_RETURN); escaped = escaped // "\r"
      case(TOML_TABULATOR); escaped = escaped // "\t"
      case(TOML_BACKSPACE); escaped = escaped // "\b"
      end select
   end do
   escaped = escaped // '"'
end subroutine reference_escape_string


end module tftest_utils

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

module tftest_ser
   use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_positive_inf, ieee_negative_inf, &
      & ieee_quiet_nan
   use testdrive
   use tomlf_build, only : get_value, set_value
   use tomlf_constants, only : tfi, tfr, tf_sp
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_de, only : toml_parse, toml_loads
   use tomlf_error, only : toml_error
   use tomlf_ser, only : toml_dumps
   use tomlf_type, only : toml_table, toml_array, new_table, add_table, add_array
   implicit none
   private

   public :: collect_ser


contains


!> Collect all exported unit tests
subroutine collect_ser(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("ser-simple-string", ser_simple_string), &
      & new_unittest("ser-integer", ser_integer), &
      & new_unittest("ser-float", ser_float), &
      & new_unittest("ser-boolean", ser_boolean), &
      & new_unittest("ser-array", ser_array), &
      & new_unittest("ser-table", ser_table), &
      & new_unittest("ser-roundtrip-simple", ser_roundtrip_simple), &
      & new_unittest("ser-real-round-trip", ser_real_round_trip), &
      & new_unittest("ser-control-characters", ser_control_characters)]

end subroutine collect_ser


!> Test serialization of simple string key-value pair
subroutine ser_simple_string(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call set_value(table, "key", "value")

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, len_trim(output) > 0, "Output should not be empty")
   if (allocated(error)) return

   call check(error, index(output, "key") > 0, "Output should contain 'key'")
end subroutine ser_simple_string


!> Test serialization of integer values
subroutine ser_integer(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call set_value(table, "int1", 42_tfi)
   call set_value(table, "int2", -123_tfi)
   call set_value(table, "int3", 0_tfi)

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "42") > 0, "Output should contain '42'")
   if (allocated(error)) return

   call check(error, index(output, "-123") > 0, "Output should contain '-123'")
end subroutine ser_integer


!> Test serialization of float values
subroutine ser_float(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call set_value(table, "pi", 3.14159_tfr)
   call set_value(table, "negative", -2.5_tfr)
   call set_value(table, "zero", 0.0_tfr)

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "3.14") > 0, "Output should contain float value")
end subroutine ser_float


!> Test serialization of boolean values
subroutine ser_boolean(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call set_value(table, "bool_true", .true.)
   call set_value(table, "bool_false", .false.)

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "true") > 0, "Output should contain 'true'")
   if (allocated(error)) return

   call check(error, index(output, "false") > 0, "Output should contain 'false'")
end subroutine ser_boolean


!> Test serialization of simple array
subroutine ser_array(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   type(toml_array), pointer :: array
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call add_array(table, "numbers", array)
   call set_value(array, 1, 1_tfi)
   call set_value(array, 2, 2_tfi)
   call set_value(array, 3, 3_tfi)

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "[") > 0, "Output should contain array bracket")
end subroutine ser_array


!> Test serialization of simple table
subroutine ser_table(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   type(toml_table), pointer :: subtable
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call add_table(table, "section", subtable)
   call set_value(subtable, "key", "value")

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "[section]") > 0, "Output should contain table header")
end subroutine ser_table


!> Test round-trip serialization (parse -> serialize -> parse)
subroutine ser_roundtrip_simple(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table), allocatable :: table1, table2
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err
   character(*), parameter :: input = 'key = "value"'//new_line("a")//"number = 42"

   ! Parse input
   call toml_loads(table1, input, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Parse failed: "//toml_err%message)
      return
   end if

   ! Serialize
   call toml_dumps(table1, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Serialize failed: "//toml_err%message)
      return
   end if

   ! Parse again
   call toml_loads(table2, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Re-parse failed: "//toml_err%message)
      return
   end if

   call check(error, allocated(table2), "Round-trip should produce valid table")
end subroutine ser_roundtrip_simple


!> A real is written with enough digits to read back bit for bit at every magnitude, and a
!> zero keeps its sign: every binary decade of a real64 and of a real32, both signs, from
!> the smallest subnormal to huge, and a few values of note
subroutine ser_real_round_trip(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   type(toml_table), allocatable :: reread
   type(toml_error), allocatable :: toml_err
   character(:), allocatable :: output
   real(tfr), allocatable :: dval(:)
   real(tf_sp), allocatable :: sval(:)
   character(16) :: key
   character(32) :: text
   real(tfr) :: zero, a, b, dread
   real(tf_sp) :: szero, sread
   integer :: it, k, nd, ns, stat
   logical :: same

   zero = 0.0_tfr
   a = 0.1_tfr
   b = 0.2_tfr
   allocate(dval(2*(maxexponent(zero) - minexponent(zero) + digits(zero) + 1) + 9))
   nd = 0
   do k = minexponent(zero) - digits(zero), maxexponent(zero) - 1
      dval(nd+1) = scale(1.0_tfr, k)
      dval(nd+2) = -scale(1.0_tfr, k)
      nd = nd + 2
   end do
   dval(nd+1:nd+9) = [huge(zero), -huge(zero), a + b, 1.0e-20_tfr, zero, -zero, &
      & ieee_value(zero, ieee_positive_inf), ieee_value(zero, ieee_negative_inf), &
      & ieee_value(zero, ieee_quiet_nan)]
   nd = nd + 9

   szero = 0.0_tf_sp
   allocate(sval(2*(maxexponent(szero) - minexponent(szero) + digits(szero) + 1) + 4))
   ns = 0
   do k = minexponent(szero) - digits(szero), maxexponent(szero) - 1
      sval(ns+1) = scale(1.0_tf_sp, k)
      sval(ns+2) = -scale(1.0_tf_sp, k)
      ns = ns + 2
   end do
   sval(ns+1:ns+4) = [huge(szero), -huge(szero), szero, -szero]
   ns = ns + 4

   call new_table(table)
   do it = 1, nd
      write(key, '("d", i0)') it
      call set_value(table, trim(key), dval(it))
   end do
   do it = 1, ns
      write(key, '("s", i0)') it
      call set_value(table, trim(key), sval(it))
   end do

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Serialize failed: "//toml_err%message)
      return
   end if
   call toml_loads(reread, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Re-parse failed: "//toml_err%message)
      return
   end if

   do it = 1, nd
      write(key, '("d", i0)') it
      call get_value(reread, trim(key), dread, stat=stat)
      if (dval(it) /= dval(it)) then
         same = stat == 0 .and. dread /= dread
      else
         same = stat == 0 .and. transfer(dread, 0_tfi) == transfer(dval(it), 0_tfi)
      end if
      if (.not.same) then
         write(text, '(es24.16e3)') dval(it)
         call test_failed(error, "real64 "//trim(adjustl(text))//" must read back bit for bit")
         return
      end if
   end do
   do it = 1, ns
      write(key, '("s", i0)') it
      call get_value(reread, trim(key), sread, stat=stat)
      if (.not.(stat == 0 .and. transfer(sread, 0) == transfer(sval(it), 0))) then
         write(text, '(es16.8e3)') sval(it)
         call test_failed(error, "real32 "//trim(adjustl(text))//" must read back bit for bit")
         return
      end if
   end do
end subroutine ser_real_round_trip


!> Every control character is written so that the text is TOML and reads back to the same
!> byte, in a string value and in a key. Each backslash is achar(92).
subroutine ser_control_characters(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(*), parameter :: bs = achar(92), nl = new_line("a")
   type(toml_table) :: table
   type(toml_table), allocatable :: loaded, reread
   type(toml_error), allocatable :: toml_err
   character(:), allocatable :: output, sval
   character(16) :: key
   integer :: codes(33), it, stat
   integer(tfi) :: ival

   codes = [(it, it = 0, 31), 127]
   call new_table(table)
   do it = 1, size(codes)
      write(key, '("v", i0)') codes(it)
      call set_value(table, trim(key), achar(codes(it)))
      call set_value(table, achar(codes(it)), int(codes(it), tfi))
   end do

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Serialize failed: "//toml_err%message)
      return
   end if
   call toml_loads(reread, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Re-parse failed: "//toml_err%message)
      return
   end if

   do it = 1, size(codes)
      write(key, '("v", i0)') codes(it)
      call get_value(reread, trim(key), sval, stat=stat)
      call check(error, stat == 0 .and. sval == achar(codes(it)) .and. len(sval) == 1, &
         & "a string holding byte "//trim(key(2:))//" must read back")
      if (allocated(error)) return
      call get_value(reread, achar(codes(it)), ival, stat=stat)
      call check(error, stat == 0 .and. ival == codes(it), &
         & "a key holding byte "//trim(key(2:))//" must read back")
      if (allocated(error)) return
   end do

   ! Escapes read from a valid document must survive being written and read again
   call toml_loads(loaded, 'x = "a'//bs//'u0001b"'//nl//'"k'//bs//'u0007" = 1'//nl, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Parse failed: "//toml_err%message)
      return
   end if
   call toml_dumps(loaded, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Serialize failed: "//toml_err%message)
      return
   end if
   call toml_loads(reread, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Re-parse failed: "//toml_err%message)
      return
   end if
   call get_value(reread, "x", sval, stat=stat)
   call check(error, stat == 0 .and. sval == "a"//achar(1)//"b", "x must read back as a, 01, b")
   if (allocated(error)) return
   call get_value(reread, "k"//achar(7), ival, stat=stat)
   call check(error, stat == 0 .and. ival == 1, "the key k, 07 must read back")
end subroutine ser_control_characters


end module tftest_ser

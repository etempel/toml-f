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

module tomlf_utils
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, tf_sp, tf_dp, &
      & TOML_NEWLINE, TOML_FORMFEED, TOML_CARRIAGE_RETURN, TOML_TABULATOR, TOML_BACKSPACE, &
      & TOML_BAREKEY
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time, to_string
   use tomlf_utils_io, only : read_whole_file, read_whole_line
   implicit none
   private

   public :: toml_escape_string
   public :: valid_utf8, show_invalid_utf8
   public :: to_string
   public :: read_whole_file, read_whole_line


   interface to_string
      module procedure :: to_string_i1
      module procedure :: to_string_i2
      module procedure :: to_string_i4
      module procedure :: to_string_i8
      module procedure :: to_string_r8
   end interface to_string


contains


!> Escape all special characters in a TOML string
subroutine toml_escape_string(raw, escaped, multiline)

   !> Raw representation of TOML string
   character(kind=tfc, len=*), intent(in) :: raw

   !> Escaped view of the TOML string
   character(kind=tfc, len=:), allocatable, intent(out) :: escaped

   !> Preserve newline characters
   logical, intent(in), optional :: multiline

   character(*, tfc), parameter :: hexdigit = "0123456789ABCDEF"
   character(2, tfc) :: short
   integer :: i, k, n, code
   logical :: preserve_newline

   preserve_newline = .false.
   if (present(multiline)) preserve_newline = multiline

   ! Size the result in one pass and fill it in a second: appending one character at a
   ! time would copy everything written so far, for time growing with the square of the length
   n = 2
   do i = 1, len(raw)
      select case(raw(i:i))
      case default
         n = n + 1
      case(achar(0):achar(7), achar(11), achar(14):achar(31), achar(127))
         n = n + 6
      case("\", '"', TOML_FORMFEED, TOML_CARRIAGE_RETURN, TOML_TABULATOR, TOML_BACKSPACE)
         n = n + 2
      case(TOML_NEWLINE)
         n = n + merge(1, 2, preserve_newline)
      end select
   end do

   allocate(character(kind=tfc, len=n) :: escaped)
   escaped(1:1) = '"'
   k = 1
   do i = 1, len(raw)
      select case(raw(i:i))
      case default
         escaped(k+1:k+1) = raw(i:i)
         k = k + 1
         cycle
      case(achar(0):achar(7), achar(11), achar(14):achar(31), achar(127))
         ! TOML forbids these control characters unescaped, and has no short escape for them
         code = iachar(raw(i:i))
         escaped(k+1:k+6) = "\u00" // hexdigit(code/16+1:code/16+1) // hexdigit(mod(code, 16)+1:mod(code, 16)+1)
         k = k + 6
         cycle
      case(TOML_NEWLINE)
         if (preserve_newline) then
            escaped(k+1:k+1) = raw(i:i)
            k = k + 1
            cycle
         end if
         short = "\n"
      case("\"); short = "\\"
      case('"'); short = '\"'
      case(TOML_FORMFEED); short = "\f"
      case(TOML_CARRIAGE_RETURN); short = "\r"
      case(TOML_TABULATOR); short = "\t"
      case(TOML_BACKSPACE); short = "\b"
      end select
      escaped(k+1:k+2) = short
      k = k + 2
   end do
   escaped(n:n) = '"'

end subroutine toml_escape_string


!> Whether text is well-formed UTF-8 (RFC 3629): no overlong form, no surrogate, nothing
!> above U+10FFFF. The lexer and the serialiser both use it, so that the reader and the
!> writer accept exactly the same text.
pure function valid_utf8(text) result(valid)

   !> Bytes to check
   character(*, tfc), intent(in) :: text

   !> Whether every byte sequence is a valid UTF-8 encoding of a scalar value
   logical :: valid

   integer :: it, nchar

   valid = .true.
   it = 1
   do while(it <= len(text))
      ! Most text is ASCII: take it a byte at a time without the full decoding
      if (byte_value(text(it:it)) < 128) then
         it = it + 1
         cycle
      end if
      nchar = utf8_char_len(text, it)
      if (nchar == 0) then
         valid = .false.
         return
      end if
      it = it + nchar
   end do

end function valid_utf8


!> Text with each byte that is not part of a valid UTF-8 character written as \xHH (hex
!> digits in upper case), and every valid character as it is, for an error message
pure function show_invalid_utf8(text) result(shown)

   !> Bytes to show
   character(*, tfc), intent(in) :: text

   !> Text to show
   character(kind=tfc, len=:), allocatable :: shown

   character(*, tfc), parameter :: hexdigit = "0123456789ABCDEF"
   integer :: it, k, n, nchar, b

   ! Size the result in one pass and fill it in a second
   n = 0
   it = 1
   do while(it <= len(text))
      nchar = utf8_char_len(text, it)
      if (nchar == 0) then
         n = n + 4
         it = it + 1
      else
         n = n + nchar
         it = it + nchar
      end if
   end do

   allocate(character(kind=tfc, len=n) :: shown)
   k = 0
   it = 1
   do while(it <= len(text))
      nchar = utf8_char_len(text, it)
      if (nchar == 0) then
         b = byte_value(text(it:it))
         shown(k+1:k+4) = "\x" // hexdigit(b/16+1:b/16+1) // hexdigit(mod(b, 16)+1:mod(b, 16)+1)
         k = k + 4
         it = it + 1
      else
         shown(k+1:k+nchar) = text(it:it+nchar-1)
         k = k + nchar
         it = it + nchar
      end if
   end do

end function show_invalid_utf8


!> Length in bytes of the valid UTF-8 character starting at position pos of text, or 0 when
!> the bytes there are not one: no overlong form, no surrogate, nothing above U+10FFFF
pure function utf8_char_len(text, pos) result(nchar)

   !> Bytes holding the character
   character(*, tfc), intent(in) :: text

   !> Position of the character's first byte, 1 to len(text)
   integer, intent(in) :: pos

   !> Length of the character, 1 to 4, or 0 when it is not valid
   integer :: nchar

   integer :: b, ncont, lo, hi, k

   nchar = 0
   b = byte_value(text(pos:pos))
   ! Range of the first continuation byte; every later one is 80 to BF
   lo = 128
   hi = 191
   select case(b)
   case(0:127)
      nchar = 1
      return
   case(194:223)
      ncont = 1
   case(224)
      ! No overlong three-byte form
      ncont = 2
      lo = 160
   case(225:236, 238:239)
      ncont = 2
   case(237)
      ! No surrogate, D800 to DFFF
      ncont = 2
      hi = 159
   case(240)
      ! No overlong four-byte form
      ncont = 3
      lo = 144
   case(241:243)
      ncont = 3
   case(244)
      ! Nothing above 10FFFF
      ncont = 3
      hi = 143
   case default
      ! A continuation byte alone, an overlong two-byte form (C0, C1), or F5 to FF
      return
   end select
   if (pos + ncont > len(text)) return
   do k = 1, ncont
      b = byte_value(text(pos+k:pos+k))
      if (b < lo .or. b > hi) return
      lo = 128
      hi = 191
   end do
   nchar = ncont + 1

end function utf8_char_len


!> The byte a character holds, 0 to 255 (ichar may answer a negative number above 127)
elemental function byte_value(ch) result(b)

   !> One character
   character(1, tfc), intent(in) :: ch

   !> Its byte, 0 to 255
   integer :: b

   b = ichar(ch)
   if (b < 0) b = b + 256

end function byte_value


!> Represent an integer as character sequence.
pure function to_string_i1(val) result(string)
   integer, parameter :: ik = tf_i1
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do

   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function to_string_i1


!> Represent an integer as character sequence.
pure function to_string_i2(val) result(string)
   integer, parameter :: ik = tf_i2
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do

   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function to_string_i2


!> Represent an integer as character sequence.
pure function to_string_i4(val) result(string)
   integer, parameter :: ik = tf_i4
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do

   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function to_string_i4


!> Represent an integer as character sequence.
pure function to_string_i8(val) result(string)
   integer, parameter :: ik = tf_i8
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do

   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function to_string_i8

!> Represent an real as character sequence.
pure function to_string_r8(val) result(string)
   integer, parameter :: rk = tfr
   !> Real value to create string from
   real(rk), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   character(128, tfc) :: buffer

   ! NaN first: an ordered comparison on a NaN signals IEEE invalid on some processors
   ! (gfortran), where /= is quiet
   if (val /= val) then
      string = "nan"
   else if (val > huge(val)) then
      string = "+inf"
   else if (val < -huge(val)) then
      string = "-inf"
   else if (val == 0.0_rk) then
      ! A zero, or under abrupt underflow (flush-to-zero and denormals-are-zero, as in ifx's
      ! default build) a subnormal, which then compares equal to zero. The text ES editing gives
      ! tells them apart: a test of the bits would too, but an optimiser that assumes gradual
      ! underflow turns that test back into val == 0.
      write(buffer, "(es24.16e3)") val
      if (scan(buffer(:index(buffer, "E")), "123456789") > 0) then
         string = trim(adjustl(buffer))
      ! The sign from the bit itself: some processors (ifx) drop a zero's sign in F and ES editing
      else if (transfer(val, 0_tfi) < 0_tfi) then
         string = "-0.0"
      else
         string = "0.0"
      end if
   else
      if (abs(val) >= 1.0e+100_rk) then
         write(buffer, "(es24.16e3)") val
      else if (abs(val) >= 1.0e+10_rk) then
         write(buffer, "(es24.16e2)") val
      else if (abs(val) >= 1.0e+3_rk) then
         write(buffer, "(es24.16e1)") val
      else if (abs(val) >= 1.0_rk) then
         write(buffer, "(f24.16)") val
      else
         ! Below 1, f24.16 keeps one significant digit fewer per decade, and none below
         ! about 5e-17: write seventeen significant digits whatever the magnitude
         write(buffer, "(es24.16e3)") val
      end if
      string = trim(adjustl(buffer))
   end if
end function to_string_r8

end module tomlf_utils

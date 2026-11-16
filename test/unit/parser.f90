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

module tftest_parser
   use testdrive
   use tomlf_build, only : get_value
   use tomlf_constants, only : nl => TOML_NEWLINE, tfi, tfr
   use tomlf_de, only : toml_loads
   use tomlf_de_lexer, only : toml_lexer, new_lexer_from_string, toml_token, token_kind
   use tomlf_de_parser
   use tomlf_error, only : toml_error
   use tomlf_terminal, only : toml_terminal
   use tomlf_type, only : toml_table
   implicit none
   private

   public :: collect_parser

   type, extends(toml_lexer) :: mocked_lexer
      type(toml_token), allocatable :: token(:)
   contains
      procedure :: next
   end type mocked_lexer

contains

!> Collect all exported unit tests
subroutine collect_parser(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("root-invalid", root_invalid, should_fail=.true.), &
      & new_unittest("table-body", table_body), &
      & new_unittest("table-header-long", table_header_long), &
      & new_unittest("table-header-empty", table_header_empty, should_fail=.true.), &
      & new_unittest("table-header-unclosed1", table_header_unclosed1, should_fail=.true.), &
      & new_unittest("table-header-unclosed2", table_header_unclosed2, should_fail=.true.), &
      & new_unittest("table-header-invalid", table_header_invalid, should_fail=.true.), &
      & new_unittest("table-header-newline1", table_header_newline1), &
      & new_unittest("table-header-newline2", table_header_newline2, should_fail=.true.), &
      & new_unittest("table-header-duplicate", table_header_duplicate, should_fail=.true.), &
      & new_unittest("table-header-trailing-whitespace", table_header_trailing_whitespace), &
      & new_unittest("table-header-trailing-comment", table_header_trailing_comment), &
      & new_unittest("aot-body", aot_body), &
      & new_unittest("aot-inline", aot_inline, should_fail=.true.), &
      & new_unittest("aot-header-whitespace1", aot_header_whitespace1), &
      & new_unittest("aot-header-whitespace2", aot_header_whitespace2), &
      & new_unittest("aot-header-empty", aot_header_empty, should_fail=.true.), &
      & new_unittest("aot-header-unclosed1", aot_header_unclosed1, should_fail=.true.), &
      & new_unittest("aot-header-unclosed2", aot_header_unclosed2, should_fail=.true.), &
      & new_unittest("aot-header-unclosed3", aot_header_unclosed3, should_fail=.true.), &
      & new_unittest("aot-header-duplicate1", aot_header_duplicate1, should_fail=.true.), &
      & new_unittest("aot-header-duplicate2", aot_header_duplicate2, should_fail=.true.), &
      & new_unittest("aot-header-duplicate3", aot_header_duplicate3, should_fail=.true.), &
      & new_unittest("aot-header-invalid1", aot_header_invalid1, should_fail=.true.), &
      & new_unittest("aot-header-invalid2", aot_header_invalid2, should_fail=.true.), &
      & new_unittest("keyval-empty", keyval_empty, should_fail=.true.), &
      & new_unittest("keyval-equal", keyval_equal, should_fail=.true.), &
      & new_unittest("keyval-dotted", keyval_dotted), &
      & new_unittest("keyval-comment", keyval_comment), &
      & new_unittest("inline-array", inline_array), &
      & new_unittest("inline-array-empty", inline_array_empty), &
      & new_unittest("inline-array-whitespace", inline_array_whitespace), &
      & new_unittest("inline-array-newline", inline_array_newline), &
      & new_unittest("inline-array-nested", inline_array_nested), &
      & new_unittest("inline-table", inline_table), &
      & new_unittest("inline-table-empty", inline_table_empty), &
      & new_unittest("inline-table-whitespace", inline_table_whitespace), &
      & new_unittest("inline-table-newline", inline_table_newline), &
      & new_unittest("inline-table-comma", inline_table_comma), &
      & new_unittest("inline-table-modify", inline_table_modify, should_fail=.true.), &
      ! new_unittest("dotted-key-non-table", dotted_key_non_table, should_fail=.true.), &
      ! new_unittest("dotted-key-array", dotted_key_array, should_fail=.true.), &
      & new_unittest("parse-escaped-key", parse_escaped_key), &
      & new_unittest("parse-multiline-string", parse_multiline_string), &
      & new_unittest("parse-multiline-string-blank-lines", parse_multiline_string_blank_lines), &
      & new_unittest("empty", empty), &
      & new_unittest("integer-out-of-range", integer_out_of_range), &
      & new_unittest("float-out-of-range", float_out_of_range), &
      & new_unittest("multiline-unicode-escape", multiline_unicode_escape), &
      & new_unittest("unicode-scalar-values", unicode_scalar_values), &
      & new_unittest("multiline-crlf", multiline_crlf), &
      & new_unittest("header-extends-inline-array", header_extends_inline_array), &
      & new_unittest("invalid-utf8", invalid_utf8), &
      & new_unittest("comment-control-characters", comment_control_characters)]

end subroutine collect_parser

subroutine parse_escaped_key(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table), allocatable :: table
   type(toml_error), allocatable :: toml_err

   call toml_loads(table, '"\\n" = "newline"', error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, allocated(table), "Escaped newline key should parse")
end subroutine parse_escaped_key

subroutine parse_multiline_string(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table), allocatable :: table
   type(toml_error), allocatable :: toml_err
   character(:), allocatable :: value
   integer :: stat
   character(*), parameter :: input = 'equivalent_three = """\' // new_line("a") // &
      & "       The quick brown \" // new_line("a") // &
      & "       fox jumps over \" // new_line("a") // &
      & '       the lazy dog."""'

   call toml_loads(table, input, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call get_value(table, "equivalent_three", value, stat=stat)
   call check(error, stat == 0, "Multiline string should decode")
   if (allocated(error)) return
   call check(error, value == "The quick brown fox jumps over the lazy dog.", &
      & "Multiline string should collapse line continuations")
end subroutine parse_multiline_string

subroutine parse_multiline_string_blank_lines(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table), allocatable :: table
   type(toml_error), allocatable :: toml_err
   character(:), allocatable :: value
   integer :: stat
   character(*), parameter :: input = 'equivalent_two = """' // new_line("a") // &
      & "The quick brown \" // new_line("a") // &
      & new_line("a") // &
      & "  fox jumps over \" // new_line("a") // &
      & '    the lazy dog."""'

   call toml_loads(table, input, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call get_value(table, "equivalent_two", value, stat=stat)
   call check(error, stat == 0, "Multiline string with blank lines should decode")
   if (allocated(error)) return
   call check(error, value == "The quick brown fox jumps over the lazy dog.", &
      & "Multiline line continuations should remove blank-line whitespace")
end subroutine parse_multiline_string_blank_lines

subroutine empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "", [toml_token(token_kind%eof)])
end subroutine empty

subroutine root_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "}", &
      & [toml_token(token_kind%lbrace, 1, 1), toml_token(token_kind%eof, 2, 2)])
end subroutine root_invalid

subroutine table_body(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a]"//nl//"c=1"//nl//"[b]"//nl//"c=1", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%newline, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%newline, 8, 8), &
      &  toml_token(token_kind%lbracket, 9, 9), toml_token(token_kind%keypath, 10, 10), &
      &  toml_token(token_kind%rbracket, 11, 11), toml_token(token_kind%newline, 12, 12), &
      &  toml_token(token_kind%keypath, 13, 13), toml_token(token_kind%equal, 14, 14), &
      &  toml_token(token_kind%int, 15, 15), toml_token(token_kind%eof, 16, 16)])
end subroutine table_body

subroutine aot_body(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a]]"//nl//"b=1"//nl//"[[a]]"//nl//"b=1", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%newline, 6, 6), &
      &  toml_token(token_kind%keypath, 7, 7), toml_token(token_kind%equal, 8, 8), &
      &  toml_token(token_kind%int, 9, 9), toml_token(token_kind%newline, 10, 10), &
      &  toml_token(token_kind%lbracket, 11, 11), toml_token(token_kind%lbracket, 12, 12), &
      &  toml_token(token_kind%keypath, 13, 13), toml_token(token_kind%rbracket, 14, 14), &
      &  toml_token(token_kind%rbracket, 15, 15), toml_token(token_kind%newline, 16, 16), &
      &  toml_token(token_kind%keypath, 17, 17), toml_token(token_kind%equal, 18, 18), &
      &  toml_token(token_kind%int, 19, 19), toml_token(token_kind%eof, 20, 20)])
end subroutine aot_body

subroutine table_header_long(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%dot, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%dot, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%dot, 9, 9), toml_token(token_kind%keypath, 10, 10), &
      &  toml_token(token_kind%dot, 11, 11), toml_token(token_kind%keypath, 12, 12), &
      &  toml_token(token_kind%dot, 13, 13), toml_token(token_kind%keypath, 14, 14), &
      &  toml_token(token_kind%dot, 15, 15), toml_token(token_kind%keypath, 16, 16), &
      &  toml_token(token_kind%dot, 17, 17), toml_token(token_kind%keypath, 18, 18), &
      &  toml_token(token_kind%dot, 19, 19), toml_token(token_kind%keypath, 20, 20), &
      &  toml_token(token_kind%dot, 21, 21), toml_token(token_kind%keypath, 22, 22), &
      &  toml_token(token_kind%dot, 23, 23), toml_token(token_kind%keypath, 24, 24), &
      &  toml_token(token_kind%dot, 25, 25), toml_token(token_kind%keypath, 26, 26), &
      &  toml_token(token_kind%dot, 27, 27), toml_token(token_kind%keypath, 28, 28), &
      &  toml_token(token_kind%dot, 29, 29), toml_token(token_kind%keypath, 30, 30), &
      &  toml_token(token_kind%dot, 31, 31), toml_token(token_kind%keypath, 32, 32), &
      &  toml_token(token_kind%dot, 33, 33), toml_token(token_kind%keypath, 34, 34), &
      &  toml_token(token_kind%dot, 35, 35), toml_token(token_kind%keypath, 36, 36), &
      &  toml_token(token_kind%dot, 37, 37), toml_token(token_kind%keypath, 38, 38), &
      &  toml_token(token_kind%dot, 39, 39), toml_token(token_kind%keypath, 40, 40), &
      &  toml_token(token_kind%dot, 41, 41), toml_token(token_kind%keypath, 42, 42), &
      &  toml_token(token_kind%dot, 43, 43), toml_token(token_kind%keypath, 44, 44), &
      &  toml_token(token_kind%dot, 45, 45), toml_token(token_kind%keypath, 46, 46), &
      &  toml_token(token_kind%dot, 47, 47), toml_token(token_kind%keypath, 48, 48), &
      &  toml_token(token_kind%dot, 49, 49), toml_token(token_kind%keypath, 50, 50), &
      &  toml_token(token_kind%dot, 51, 51), toml_token(token_kind%keypath, 52, 52), &
      &  toml_token(token_kind%rbracket, 53, 53), toml_token(token_kind%eof, 54, 54)])
end subroutine table_header_long

subroutine table_header_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[]", &
      & [toml_token(token_kind%lbracket, 60, 1), toml_token(token_kind%rbracket, 2, 2), &
      &  toml_token(token_kind%eof, 15, 9)])
end subroutine table_header_empty

subroutine table_header_unclosed1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%eof, 2, 2)])
end subroutine table_header_unclosed1

subroutine table_header_unclosed2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine table_header_unclosed2

subroutine table_header_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a,]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%comma, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine table_header_invalid

subroutine table_header_newline1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[""\n""]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%string, 2, 5), &
      &  toml_token(token_kind%rbracket, 6, 6), toml_token(token_kind%eof, 7, 7)])
end subroutine table_header_newline1

subroutine table_header_newline2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a] [b]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%lbracket, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%rbracket, 7, 7), toml_token(token_kind%eof, 8, 8)])
end subroutine table_header_newline2

subroutine table_header_duplicate(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a]"//nl//"[a]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%newline, 4, 4), &
      &  toml_token(token_kind%lbracket, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%rbracket, 7, 7), toml_token(token_kind%eof, 5, 5)])
end subroutine table_header_duplicate

subroutine table_header_trailing_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.b] ", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%whitespace, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine table_header_trailing_whitespace

subroutine table_header_trailing_comment(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.b]#", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%comment, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine table_header_trailing_comment

subroutine aot_header_whitespace1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[ a]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%whitespace, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine aot_header_whitespace1

subroutine aot_header_whitespace2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a ]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine aot_header_whitespace2

subroutine aot_header_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine aot_header_empty

subroutine aot_header_unclosed1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%eof, 3, 3)])
end subroutine aot_header_unclosed1

subroutine aot_header_unclosed2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a.]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%dot, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine aot_header_unclosed2

subroutine aot_header_unclosed3(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[ [a]  #", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%whitespace, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%whitespace, 6, 7), &
      &  toml_token(token_kind%comment, 8, 8), toml_token(token_kind%eof, 9, 9)])
end subroutine aot_header_unclosed3

subroutine aot_header_duplicate1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a]]"//nl//"[a]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%newline, 6, 6), &
      &  toml_token(token_kind%lbracket, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine aot_header_duplicate1

subroutine aot_header_duplicate2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a]"//nl//"[[a]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%newline, 4, 4), &
      &  toml_token(token_kind%lbracket, 5, 5), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%keypath, 7, 7), toml_token(token_kind%rbracket, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine aot_header_duplicate2

subroutine aot_header_duplicate3(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.b.c]"//nl//"[[a.b]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%dot, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%rbracket, 7, 7), toml_token(token_kind%newline, 8, 8), &
      &  toml_token(token_kind%lbracket, 9, 9), toml_token(token_kind%lbracket, 10, 10), &
      &  toml_token(token_kind%keypath, 11, 11), toml_token(token_kind%dot, 12, 12), &
      &  toml_token(token_kind%keypath, 13, 13), toml_token(token_kind%rbracket, 14, 14), &
      &  toml_token(token_kind%rbracket, 15, 15), toml_token(token_kind%eof, 16, 16)])
end subroutine aot_header_duplicate3

subroutine aot_header_invalid1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[ [a]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%whitespace, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 6, 6)])
end subroutine aot_header_invalid1

subroutine aot_header_invalid2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a] ]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%whitespace, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 6, 6)])
end subroutine aot_header_invalid2

subroutine aot_inline(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[]"//nl//"[[a]]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%newline, 6, 6), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%lbracket, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%rbracket, 10, 10), &
      &  toml_token(token_kind%eof, 6, 6)])
end subroutine aot_inline

subroutine keyval_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%eof, 3, 3)])
end subroutine keyval_empty

subroutine keyval_equal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=,", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%comma, 3, 3), toml_token(token_kind%eof, 4, 4)])
end subroutine keyval_equal

subroutine keyval_dotted(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a.b.c=1", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%dot, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%dot, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%eof, 8, 8)])
end subroutine keyval_dotted

subroutine keyval_comment(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "# This is a full-line comment"//nl//&
      & "key = ""value"" # This is a comment at the end of a line", &
      & [toml_token(token_kind%comment, 1, 29), toml_token(token_kind%newline, 30, 30), &
      &  toml_token(token_kind%keypath, 31, 33), toml_token(token_kind%whitespace, 34, 34), &
      &  toml_token(token_kind%equal, 35, 35), toml_token(token_kind%whitespace, 36, 36), &
      &  toml_token(token_kind%string, 37, 43), toml_token(token_kind%whitespace, 44, 44), &
      &  toml_token(token_kind%comment, 45, 85), toml_token(token_kind%eof, 86, 86)])
end subroutine keyval_comment

subroutine inline_array(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[1,[2,],[3]]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%int, 4, 4), &
      &  toml_token(token_kind%comma, 5, 5), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%comma, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%comma, 10, 10), &
      &  toml_token(token_kind%lbracket, 11, 11), toml_token(token_kind%int, 12, 12), &
      &  toml_token(token_kind%rbracket, 13, 13), toml_token(token_kind%rbracket, 14, 14), &
      &  toml_token(token_kind%eof, 15, 15)])
end subroutine inline_array

subroutine inline_array_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine inline_array_empty

subroutine inline_array_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[ 1 , ]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%int, 5, 5), toml_token(token_kind%whitespace, 6, 6), &
      &  toml_token(token_kind%comma, 7, 7), toml_token(token_kind%whitespace, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine inline_array_whitespace

subroutine inline_array_newline(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[ #"//nl//"1 #"//nl//", #"//nl//"] #", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%comment, 5, 5), toml_token(token_kind%newline, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%whitespace, 8, 8), &
      &  toml_token(token_kind%comment, 9, 9), toml_token(token_kind%newline, 10, 10), &
      &  toml_token(token_kind%comma, 11, 11), toml_token(token_kind%whitespace, 12, 12), &
      &  toml_token(token_kind%comment, 13, 13), toml_token(token_kind%newline, 14, 14), &
      &  toml_token(token_kind%rbracket, 15, 15), toml_token(token_kind%whitespace, 16, 16), &
      &  toml_token(token_kind%comment, 17, 17), toml_token(token_kind%eof, 18, 18)])
end subroutine inline_array_newline


subroutine inline_array_nested(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "data=[[""gamma"",""delta""],[1,2]]", &
      & [toml_token(token_kind%keypath, 1, 4), toml_token(token_kind%equal, 5, 5), &
      &  toml_token(token_kind%lbracket, 6, 6), toml_token(token_kind%lbracket, 7, 7), &
      &  toml_token(token_kind%string, 8, 14), toml_token(token_kind%comma, 15, 15), &
      &  toml_token(token_kind%string, 16, 22), toml_token(token_kind%rbracket, 23, 23), &
      &  toml_token(token_kind%comma, 24, 24), toml_token(token_kind%lbracket, 25, 25), &
      &  toml_token(token_kind%int, 26, 26), toml_token(token_kind%comma, 27, 27), &
      &  toml_token(token_kind%int, 28, 28), toml_token(token_kind%rbracket, 29, 29), &
      &  toml_token(token_kind%rbracket, 30, 30), toml_token(token_kind%eof, 31, 31)])
end subroutine inline_array_nested

subroutine inline_table(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={b=1,c.d=2}", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%equal, 5, 5), toml_token(token_kind%int, 6, 6), &
      &  toml_token(token_kind%comma, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%dot, 9, 9), toml_token(token_kind%keypath, 10, 10), &
      &  toml_token(token_kind%equal, 11, 11), toml_token(token_kind%int, 12, 12), &
      &  toml_token(token_kind%rbrace, 13, 13), toml_token(token_kind%eof, 14, 14)])
end subroutine inline_table

subroutine inline_table_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={}", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%rbrace, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine inline_table_empty

subroutine inline_table_empty_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={ }", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%rbrace, 5, 5), toml_token(token_kind%eof, 6, 6)])
end subroutine inline_table_empty_whitespace

subroutine inline_table_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={ b=1 }", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%whitespace, 8, 8), &
      &  toml_token(token_kind%rbrace, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine inline_table_whitespace

subroutine inline_table_newline(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={ b=1"//nl//"}", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%newline, 8, 8), &
      &  toml_token(token_kind%rbrace, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine inline_table_newline

subroutine inline_table_comma(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={ b=1,}", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%comma, 8, 8), &
      &  toml_token(token_kind%rbrace, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine inline_table_comma


subroutine inline_table_modify(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={}"//nl//"[a.b]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%rbrace, 4, 4), &
      &  toml_token(token_kind%newline, 5, 5), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%keypath, 7, 7), toml_token(token_kind%dot, 8, 8), &
      &  toml_token(token_kind%keypath, 9, 9), toml_token(token_kind%rbracket, 10, 10), &
      &  toml_token(token_kind%eof, 11, 11)])
end subroutine inline_table_modify

subroutine dotted_key_non_table(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=1"//nl//"[a.b]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%int, 3, 3), toml_token(token_kind%newline, 4, 4), &
      &  toml_token(token_kind%lbracket, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%dot, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine dotted_key_non_table

subroutine dotted_key_array(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[]"//nl//"[a.b]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%newline, 5, 5), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%keypath, 7, 7), toml_token(token_kind%dot, 8, 8), &
      &  toml_token(token_kind%keypath, 9, 9), toml_token(token_kind%rbracket, 10, 10), &
      &  toml_token(token_kind%eof, 11, 11)])
end subroutine dotted_key_array

subroutine check_parser(error, string, token)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error
   character(len=*), intent(in) :: string
   type(toml_token), intent(in) :: token(:)

   type(mocked_lexer) :: lexer
   type(toml_error), allocatable :: parse_error
   type(toml_table), allocatable :: table

   call new_lexer(lexer, string, token)

   call parse(lexer, table, config=toml_parser_config(color=.true.), error=parse_error)
   call move_error(error, parse_error)
   if (allocated(error)) return
   call check(error, allocated(table))
end subroutine check_parser

!> Create a new lexer
subroutine new_lexer(lexer, string, token)
   type(mocked_lexer), intent(out) :: lexer
   character(len=*), intent(in) :: string
   type(toml_token), intent(in) :: token(:)

   lexer%filename = "mocked"
   lexer%pos = 0
   lexer%chunk = string
   lexer%token = token
end subroutine new_lexer

subroutine next(lexer, token)
   class(mocked_lexer), intent(inout) :: lexer
   type(toml_token), intent(inout) :: token

   lexer%pos = lexer%pos + 1
   token = lexer%token(lexer%pos)
end subroutine next

subroutine move_error(error, parse_error)
   type(error_type), allocatable, intent(out) :: error
   type(toml_error), intent(in), optional :: parse_error

   if (present(parse_error)) then
      allocate(error)
      error%stat = 1
      error%message = parse_error%message
   end if
end subroutine move_error

!> Loads text and requires it refused, with a diagnostic and no table
subroutine check_refused(error, text, what)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error
   !> TOML text
   character(*), intent(in) :: text
   !> What the text is, for the message
   character(*), intent(in) :: what

   type(toml_table), allocatable :: table
   type(toml_error), allocatable :: toml_err

   call toml_loads(table, text, error=toml_err)
   call check(error, allocated(toml_err), what//" must be refused")
end subroutine check_refused

!> Loads text and requires it accepted
subroutine check_loads(error, text, what, table)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error
   !> TOML text
   character(*), intent(in) :: text
   !> What the text is, for the message
   character(*), intent(in) :: what
   !> The document
   type(toml_table), allocatable, intent(out) :: table

   type(toml_error), allocatable :: toml_err

   call toml_loads(table, text, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, what//" must load: "//toml_err%message)
      return
   end if
   call check(error, allocated(table), what//" must give a table")
end subroutine check_loads

!> TOML 1.0: an integer that cannot be represented losslessly is an error
subroutine integer_out_of_range(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(*), parameter :: refused(6) = [character(70) :: &
      & "x = 9223372036854775808", "x = -9223372036854775809", "x = 0xffffffffffffffff", &
      & "x = 0x8000000000000000", "x = 0o1000000000000000000000", &
      & "x = 0b1"//repeat("0", 63)]
   type(toml_table), allocatable :: table
   integer(tfi) :: val
   integer :: it, stat

   do it = 1, size(refused)
      call check_refused(error, trim(refused(it)), trim(refused(it)))
      if (allocated(error)) return
   end do
   call check_loads(error, "a = 9223372036854775807"//nl//"b = -9223372036854775808"//nl// &
      & "c = 0x7fffffffffffffff"//nl//"d = 0o777777777777777777777"//nl// &
      & "e = 0b"//repeat("1", 63)//nl//"f = 0x0000000000000000001"//nl, "the int64 extremes", table)
   if (allocated(error)) return
   call get_value(table, "a", val, stat=stat)
   call check(error, stat == 0 .and. val == huge(val), "huge(int64) must read back")
   if (allocated(error)) return
   call get_value(table, "b", val, stat=stat)
   call check(error, stat == 0 .and. val == -huge(val) - 1, "-2**63 must read back")
   if (allocated(error)) return
   call get_value(table, "c", val, stat=stat)
   call check(error, stat == 0 .and. val == huge(val), "0x7fffffffffffffff must read back")
   if (allocated(error)) return
   call get_value(table, "d", val, stat=stat)
   call check(error, stat == 0 .and. val == huge(val), "0o777777777777777777777 must read back")
   if (allocated(error)) return
   call get_value(table, "e", val, stat=stat)
   call check(error, stat == 0 .and. val == huge(val), "63 binary ones must read back")
   if (allocated(error)) return
   call get_value(table, "f", val, stat=stat)
   call check(error, stat == 0 .and. val == 1, "leading zeros of a hex literal must not count")
end subroutine integer_out_of_range

!> A finite float beyond the largest double is refused; an underflow reads as a signed zero
subroutine float_out_of_range(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(*), parameter :: refused(5) = [character(40) :: &
      & "x = 1e309", "x = -1e309", "x = 1.7976931348623159e308", "x = 1e99999", &
      & "x = 1_000e306"]
   type(toml_table), allocatable :: table
   real(tfr) :: val
   integer :: it, stat

   do it = 1, size(refused)
      call check_refused(error, trim(refused(it)), trim(refused(it)))
      if (allocated(error)) return
   end do
   call check_loads(error, "a = 1.7976931348623157e308"//nl//"b = 1e-400"//nl// &
      & "c = -1e-400"//nl//"d = inf"//nl//"e = 0e99999"//nl, "floats in range", table)
   if (allocated(error)) return
   call get_value(table, "a", val, stat=stat)
   call check(error, stat == 0 .and. val == huge(val), "the largest double must read back")
   if (allocated(error)) return
   call get_value(table, "b", val, stat=stat)
   call check(error, stat == 0 .and. val == 0.0_tfr, "an underflow must read as zero")
   if (allocated(error)) return
   call get_value(table, "c", val, stat=stat)
   ! The sign bit, since sign(1.0, -0.0) is processor-dependent (ifx answers +1)
   call check(error, stat == 0 .and. val == 0.0_tfr .and. transfer(val, 1_tfi) < 0_tfi, &
      & "a negative underflow must read as -0")
   if (allocated(error)) return
   call get_value(table, "d", val, stat=stat)
   call check(error, stat == 0 .and. val > huge(val), "inf must read as an infinity")
   if (allocated(error)) return
   call get_value(table, "e", val, stat=stat)
   call check(error, stat == 0 .and. val == 0.0_tfr, "a zero significand must read as zero")
end subroutine float_out_of_range

!> A multi-line basic string's escape is validated over its own digits. Each backslash is
!> achar(92), so that no escape is written into this source as a literal.
subroutine multiline_unicode_escape(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(*), parameter :: bs = achar(92)
   type(toml_table), allocatable :: table
   character(:), allocatable :: val
   integer :: stat

   call check_loads(error, 'a = """'//bs//'u00c1abcdef"""'//nl//'b = """'//bs//'u00c1'//bs//'u00c1"""'//nl// &
      & 'c = """'//bs//'U0001F600abc"""'//nl//'d = """'//bs//'x41bc"""'//nl, &
      & "escapes in multi-line strings", table)
   if (allocated(error)) return
   call get_value(table, "a", val, stat=stat)
   call check(error, stat == 0 .and. val == achar(195)//achar(129)//"abcdef", "u00c1 then abcdef")
   if (allocated(error)) return
   call get_value(table, "b", val, stat=stat)
   call check(error, stat == 0 .and. val == repeat(achar(195)//achar(129), 2), "u00c1 twice")
   if (allocated(error)) return
   call get_value(table, "c", val, stat=stat)
   call check(error, stat == 0 .and. val == achar(240)//achar(159)//achar(152)//achar(128)//"abc", &
      & "U0001F600 then abc")
   if (allocated(error)) return
   call get_value(table, "d", val, stat=stat)
   call check(error, stat == 0 .and. val == "Abc", "x41 then bc")
   if (allocated(error)) return
   call check_refused(error, 'x = """'//bs//'ud800"""', "a surrogate in a multi-line string")
end subroutine multiline_unicode_escape

!> An escape names exactly a Unicode scalar value. Each backslash is achar(92).
subroutine unicode_scalar_values(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(*), parameter :: bs = achar(92)
   type(toml_table), allocatable :: table
   character(:), allocatable :: val
   integer :: stat

   call check_loads(error, 'a = "'//bs//'uFFFF"'//nl//'b = "'//bs//'ufffe"'//nl//'c = "'//bs//'U0010FFFF"'//nl// &
      & '"'//bs//'uFFFF" = 1'//nl//'d = """'//bs//'uFFFFx"""'//nl, "noncharacters and the last scalar value", table)
   if (allocated(error)) return
   call get_value(table, "a", val, stat=stat)
   call check(error, stat == 0 .and. val == achar(239)//achar(191)//achar(191), "uFFFF as EF BF BF")
   if (allocated(error)) return
   call check_refused(error, 'x = "'//bs//'U00110000"', "a code point above 10FFFF")
   if (allocated(error)) return
   call check_refused(error, 'x = "'//bs//'U7FFFFFFE"', "a code point far above 10FFFF")
end subroutine unicode_scalar_values

!> CR LF is a newline inside multi-line strings, and is stored as LF
subroutine multiline_crlf(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(*), parameter :: cr = achar(13), q3 = "'''"
   type(toml_table), allocatable :: table
   character(:), allocatable :: val
   integer :: stat

   call check_loads(error, 'a = """'//cr//nl//'l1'//cr//nl//'l2"""'//cr//nl// &
      & "b = "//q3//cr//nl//"l1"//cr//nl//"l2"//q3//cr//nl// &
      & 'c = """x'//achar(92)//cr//nl//'   y"""'//cr//nl, "CR LF in multi-line strings", table)
   if (allocated(error)) return
   call get_value(table, "a", val, stat=stat)
   call check(error, stat == 0 .and. val == "l1"//nl//"l2", "a basic string must read l1 LF l2")
   if (allocated(error)) return
   call get_value(table, "b", val, stat=stat)
   call check(error, stat == 0 .and. val == "l1"//nl//"l2", "a literal string must read l1 LF l2")
   if (allocated(error)) return
   call get_value(table, "c", val, stat=stat)
   call check(error, stat == 0 .and. val == "xy", "a line-ending backslash must still trim CR LF")
   if (allocated(error)) return
   call check_refused(error, 'x = """l1'//cr//'l2"""', "a bare CR in a basic string")
   if (allocated(error)) return
   call check_refused(error, "x = "//q3//"l1"//cr//"l2"//q3, "a bare CR in a literal string")
end subroutine multiline_crlf

!> A table header naming an inline array of tables is refused, not a memory fault
subroutine header_extends_inline_array(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_refused(error, "a = [{ b = 1 }]"//nl//"[a.c]"//nl, "a header into an inline array")
   if (allocated(error)) return
   call check_refused(error, "a = { b = 1 }"//nl//"[a.c]"//nl, "a header into an inline table")
end subroutine header_extends_inline_array

!> Strings, literals and comments are valid UTF-8
subroutine invalid_utf8(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table), allocatable :: table

   call check_refused(error, 'x = "a'//achar(255)//'"', "a byte FF in a string")
   if (allocated(error)) return
   call check_refused(error, "x = 'a"//achar(192)//achar(128)//"'", "an overlong NUL in a literal")
   if (allocated(error)) return
   call check_refused(error, 'x = """'//achar(237)//achar(160)//achar(128)//'"""', &
      & "an encoded surrogate in a multi-line string")
   if (allocated(error)) return
   call check_refused(error, "x = 1 # "//achar(244)//achar(144)//achar(128)//achar(128), &
      & "a code point above 10FFFF in a comment")
   if (allocated(error)) return
   call check_refused(error, 'x = "'//achar(226)//achar(130)//'"', "a truncated sequence")
   if (allocated(error)) return
   call check_loads(error, 'x = "'//achar(195)//achar(169)//achar(240)//achar(159)//achar(152)// &
      & achar(128)//'" # '//achar(226)//achar(130)//achar(172)//nl, "valid UTF-8", table)
end subroutine invalid_utf8

!> A comment holds no control character but tab
subroutine comment_control_characters(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: bad(5) = [0, 12, 16, 31, 127]
   type(toml_table), allocatable :: table
   integer :: it

   do it = 1, size(bad)
      call check_refused(error, "x = 1 # a"//achar(bad(it))//"b"//nl, "a control character in a comment")
      if (allocated(error)) return
   end do
   call check_loads(error, "x = 1 # a"//achar(9)//"b"//nl, "a tab in a comment", table)
end subroutine comment_control_characters

end module tftest_parser

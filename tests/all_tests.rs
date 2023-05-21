mod infra;

// Your tests go here!
success_tests! {
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },
    {
        name: add1,
        file: "add1.snek",
        expected: "73",
    },
    {
        name: add,
        file: "add.snek",
        expected: "15",
    },
    {
        name: nested_arith,
        file: "nested_arith.snek",
        expected: "25",
    },
    {
        name: binding,
        file: "binding.snek",
        expected: "5",
    },
    {
        name: nesting_hell1,
        file: "nesting_hell1.snek",
        expected: "1105",
    },
    {
        name: nesting_hell2,
        file: "nesting_hell2.snek",
        expected: "10546",
    },
    {
        name: nesting_hell3,
        file: "nesting_hell3.snek",
        expected: "12",
    },
    {
        name: nesting_hell4,
        file: "nesting_hell4.snek",
        expected: "100",
    },
    {
        name: shadowing,
        file: "shadowing.snek",
        expected: "6",
    },
    {
        name: many_bindings,
        file: "many_bindings.snek",
        expected: "11111111",
    },
    {
        name: sub_pos,
        file: "sub_pos.snek",
        expected: "50",
    },
    {
        name: sub_neg,
        file: "sub_neg.snek",
        expected: "-50",
    },
    {
        name: sub1,
        file: "sub1.snek",
        expected: "899",
    },
    {
        name: mult,
        file: "mult.snek",
        expected: "100",
    },
    {
        name: mult_by_one,
        file: "mult_by_one.snek",
        expected: "10",
    },
    {
        name: mult_by_zero,
        file: "mult_by_zero.snek",
        expected: "0",
    },
    {
        name: nested_mult,
        file: "nested_mult.snek",
        expected: "1000000",
    },
    {
        name: nested_add,
        file: "nested_add.snek",
        expected: "16",
    },
    {
        name: nested_sub,
        file: "nested_sub.snek",
        expected: "6",
    },
    {
        name: given_test_set,
        file: "given_test_set.snek",
        expected: "6",
    },
    {
        name: given_complex_loop,
        file: "given_complex_loop.snek",
        expected: "-6",
    },
    {
        name: given_factorial_input_1,
        file: "given_factorial_input.snek",
        input: "1",
        expected: "1",
    },
    {
        name: given_factorial_input_2,
        file: "given_factorial_input.snek",
        input: "2",
        expected: "2",
    },
    {
        name: given_factorial_input_4,
        file: "given_factorial_input.snek",
        input: "4",
        expected: "24",
    },
    {
        name: given_factorial_input_10,
        file: "given_factorial_input.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: test_1_greater_0_input,
        file: "test_if_greater0_input.snek",
        input: "1",
        expected: "true",
    },
    {
        name: test_0_greater_0_input,
        file: "test_if_greater0_input.snek",
        input: "0",
        expected: "false",
    },
    {
        name: test_999_greater_0_input,
        file: "test_if_greater0_input.snek",
        input: "999",
        expected: "true",
    },
    {
        name: test_1_greatereq_0_input,
        file: "test_if_greatereq0_input.snek",
        input: "1",
        expected: "true",
    },
    {
        name: test_0_greatereq_0_input,
        file: "test_if_greatereq0_input.snek",
        input: "0",
        expected: "true",
    },
    {
        name: test_neg1_greater_0_input,
        file: "test_if_greater0_input.snek",
        input: "-1",
        expected: "false",
    },
    {
        name: test_neg1_less_0_input,
        file: "test_if_less0_input.snek",
        input: "-1",
        expected: "true",
    },
    {
        name: test_1_less_0_input,
        file: "test_if_less0_input.snek",
        input: "1",
        expected: "false",
    },
    {
        name: test_1_lesseq_0_input,
        file: "test_if_lesseq0_input.snek",
        input: "1",
        expected: "false",
    },
    {
        name: test_0_lesseq_0_input,
        file: "test_if_lesseq0_input.snek",
        input: "0",
        expected: "true",
    },
    {
        name: isnum_input_1,
        file: "isnum_input.snek",
        input: "1",
        expected: "true",
    },
    {
        name: isnum_input_neg1,
        file: "isnum_input.snek",
        input: "-1",
        expected: "true",
    },
    {
        name: isnum_input_100,
        file: "isnum_input.snek",
        input: "100",
        expected: "true",
    },
    {
        name: isnum_input_false,
        file: "isnum_input.snek",
        input: "false",
        expected: "false",
    },
    {
        name: isnum_input_true,
        file: "isnum_input.snek",
        input: "true",
        expected: "false",
    },
    {
        name: isbool_input_true,
        file: "isbool_input.snek",
        input: "true",
        expected: "true",
    },
    {
        name: isbool_input_false,
        file: "isbool_input.snek",
        input: "false",
        expected: "true",
    },
    {
        name: isbool_input_100,
        file: "isbool_input.snek",
        input: "100",
        expected: "false",
    },
    {
        name: isbool_input_neg100,
        file: "isbool_input.snek",
        input: "-100",
        expected: "false",
    },
}

runtime_error_tests! {{
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: input_invalid_greater,
        file: "test_if_greater0_input.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: input_invalid_less,
        file: "test_if_less0_input.snek",
        input: "false",
        expected: "invalid argument",
    },
    {
        name: test_overflow,
        file: "test_overflow.snek",
        expected: "overflow",
    },
    {
        name: test_invalid_argument_add1,
        file: "test_invalid_argument_add1.snek",
        expected: "invalid argument",
    },
    {
        name: test_invalid_argument_sub1,
        file: "test_invalid_argument_sub1.snek",
        expected: "invalid argument",
    },
}

static_error_tests! {
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    },
    {
        name: unbound_set,
        file: "invalid_set.snek",
        expected: "Unbound variable identifier y",
    },
    {
        name: input_keyword_binding_fail,
        file: "input_keyword_fail.snek",
        expected: "binding overlaps with reserved keyword input",
    },
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: number_bounds_fail_neg,
        file: "number_bounds_fail_neg.snek",
        expected: "Invalid",
    },
    {
        name: unbound_id,
        file: "unbound_id.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: duplicate_binding,
        file: "duplicate_binding.snek",
        expected: "Duplicate binding",
    },
    {
        name: null_program_fail,
        file: "null_program_fail.snek",
        expected: "Invalid",
    },
    {
        name: let_nobindings_fail,
        file: "let_nobindings_fail.snek",
        expected: "Invalid",
    },
    {
        name: let_wrong_args_fail,
        file: "let_wrong_args_fail.snek",
        expected: "Invalid",
    },
    {
        name: add_wrongargs_fail,
        file: "add_wrongargs_fail.snek",
        expected: "Invalid",
    },
    {
        name: sub_wrongargs_fail,
        file: "sub_wrongargs_fail.snek",
        expected: "Invalid",
    },
    {
        name: mul_wrongargs_fail,
        file: "mul_wrongargs_fail.snek",
        expected: "Invalid",
    },
    {
        name: sub1_fail,
        file: "sub1_fail.snek",
        expected: "Invalid",
    },
    {
        name: add1_fail,
        file: "add1_fail.snek",
        expected: "Invalid",
    },
    {
        name: invalid_id_fail,
        file: "invalid_id_fail.snek",
        expected: "Invalid",
    },
    {
        name: sexp_fail,
        file: "sexp_fail.snek",
        expected: "Invalid",
    },
    {
        name: parse_block_fail,
        file: "parse_block_fail.snek",
        expected: "Invalid",
    },
    {
        name: unbound_identifier_if,
        file: "unbound_identifier_if.snek",
        expected: "Unbound variable identifier z",
    },
    {
        name: unbound_identifier_set,
        file: "unbound_identifier_set.snek",
        expected: "Unbound variable identifier z",
    },
    {
        name: unbound_identifier_break,
        file: "unbound_identifier_break.snek",
        expected: "Unbound variable identifier z",
    },
    {
        name: unbound_identifier_block,
        file: "unbound_identifier_block.snek",
        expected: "Unbound variable identifier z",
    },
}

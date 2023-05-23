use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::{hashmap, HashMap};

struct Program {
    defs: Vec<Definition>,
    main: Expr,
}

const TRUE_VAL: i64 = 7;
const FALSE_VAL: i64 = 3;

enum Definition {
    Fun1(String, String, Expr),
    Fun2(String, String, String, Expr),
    Fun3(String, String, String, String, Expr),
}

use Definition::*;
#[derive(Debug)]
enum Expr {
    Num(i32),
    True,
    False,
    Add1(Box<Expr>),
    Sub1(Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    IsNum(Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Id(String),
    Eq(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Block(Vec<Expr>),
    Break(Box<Expr>),
    Print(Box<Expr>),
    Set(String, Box<Expr>),

    Call1(String, Box<Expr>),
    Call2(String, Box<Expr>, Box<Expr>),
    Call3(String, Box<Expr>, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Pair(Box<Expr>, Box<Expr>),
    Fst(Box<Expr>),
    Snd(Box<Expr>),
    SetFst(Box<Expr>, Box<Expr>),
    SetSnd(Box<Expr>, Box<Expr>),
}

fn parse_expr(s: &Sexp) -> Expr {
        // dbg!(s);
    match s {
        Sexp::Atom(I(n)) => Expr::Num(i32::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::Add1(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::Sub1(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::IsNum(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "fst" => Expr::Fst(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "snd" => Expr::Snd(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::Plus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::Minus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "pair" => {
                Expr::Pair(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), tup, idx] if op == "index" => {
                Expr::Index(Box::new(parse_expr(tup)), Box::new(parse_expr(idx)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "tuple" => {
                let elements = exprs.iter()
                .map(|expr| parse_expr(expr))
                .collect::<Vec<Expr>>();
                Expr::Tuple(elements)
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "setfst!" => {
                Expr::SetFst(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "setsnd!" => {
                Expr::SetSnd(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "print" => Expr::Print(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::Eq(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::Lt(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                Expr::Gt(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(keyword)), Sexp::List(vec), body] if keyword == "let" => match &vec[..] {
                [Sexp::Atom(S(name)), val] => Expr::Let(
                    name.to_string(),
                    Box::new(parse_expr(val)),
                    Box::new(parse_expr(body)),
                ),
                _ => panic!("let parse error"),
            },
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),
            [Sexp::Atom(S(funname)), arg] => {
                Expr::Call1(funname.to_string(), Box::new(parse_expr(arg)))
            },
            [Sexp::Atom(S(funname)), arg1, arg2] => Expr::Call2(
                funname.to_string(),
                Box::new(parse_expr(arg1)),
                Box::new(parse_expr(arg2)),
            ),
            [Sexp::Atom(S(funname)), arg1, arg2, arg3] => Expr::Call3(
                funname.to_string(),
                Box::new(parse_expr(arg1)),
                Box::new(parse_expr(arg2)),
                Box::new(parse_expr(arg3)),
            ),

            _ => panic!("parse error: {}", s),
        },
        _ => panic!("parse error"),
    }
}

fn is_def(s: &Sexp) -> bool {
    match s {
        Sexp::List(def_vec) => match &def_vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(_), _] if keyword == "fun" => true,
            _ => false,
        },
        _ => false,
    }
}

fn parse_definition(s: &Sexp) -> Definition {
    match s {
        Sexp::List(def_vec) => match &def_vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(name_vec), body] if keyword == "fun" => {
                match &name_vec[..] {
                    [Sexp::Atom(S(funname)), Sexp::Atom(S(arg))] => {
                        Fun1(funname.to_string(), arg.to_string(), parse_expr(body))
                    }
                    [Sexp::Atom(S(funname)), Sexp::Atom(S(arg1)), Sexp::Atom(S(arg2))] => Fun2(
                        funname.to_string(),
                        arg1.to_string(),
                        arg2.to_string(),
                        parse_expr(body),
                    ),
                    [Sexp::Atom(S(funname)), Sexp::Atom(S(arg1)), Sexp::Atom(S(arg2)), Sexp::Atom(S(arg3))] => Fun3(
                        funname.to_string(),
                        arg1.to_string(),
                        arg2.to_string(),
                        arg3.to_string(),
                        parse_expr(body),
                    ),
                    _ => panic!("Bad fundef"),
                }
            }
            _ => panic!("Bad fundef"),
        },
        _ => panic!("Bad fundef"),
    }
}

fn parse_program(s: &Sexp) -> Program {
    match s {
        Sexp::List(vec) => {
            let mut defs: Vec<Definition> = vec![];
            for def_or_exp in vec {
                if is_def(def_or_exp) {
                    defs.push(parse_definition(def_or_exp));
                } else {
                    return Program {
                        defs: defs,
                        main: parse_expr(def_or_exp),
                    };
                }
            }
            panic!("Only found definitions");
        }
        _ => panic!("Program should be a list"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_expr(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    brake: &String,
    l: &mut i32,
) -> String {
    // println!("Compiling");
    match e {
        Expr::Num(n) => format!("mov rax, {}", *n << 1),
        Expr::True => format!("mov rax, {}", 3),
        Expr::False => format!("mov rax, {}", 1),
        Expr::Id(s) if s == "input" => format!("mov rax, rdi"),
        Expr::Id(s) if s == "nil" => format!("mov rax, 0x1"),
        Expr::Id(s) => {
            let offset = env.get(s).unwrap() * 8;
            format!("mov rax, [rsp + {offset}]")
        }
        Expr::Print(e) => {
            let e_is = compile_expr(e, si, env, brake, l);
            let index = if si % 2 == 1 { si + 2 } else { si + 1 };
            let offset = index * 8;
            format!(
                "
            ; Printing {e:?}
            {e_is}
            sub rsp, {offset}
            mov [rsp], rdi
            mov rdi, rax
            call snek_print
            mov rdi, [rsp]
            add rsp, {offset}
          "
            )
        }
        Expr::Set(name, val) => {
            let offset = env.get(name).unwrap() * 8;

            let save = format!("mov [rsp + {offset}], rax");
            let val_is = compile_expr(val, si, env, brake, l);
            format!(
                "
                ; setting {name} to {val:?}
              {val_is}
              {save}
              "
            )
        }
        Expr::Add1(subexpr) => compile_expr(subexpr, si, env, brake, l) + "\nadd rax, 2",
        Expr::Sub1(subexpr) => compile_expr(subexpr, si, env, brake, l) + "\nsub rax, 2",
        Expr::IsNum(e) => {
            let e_instr = compile_expr(e, si, env, brake, l);
            format!(
                "
                ; Checking if {e:?} is a number
                {e_instr}
                and rax, 1
                cmp rax, 0
                mov rbx, {TRUE_VAL}
                mov rax, {FALSE_VAL}
                cmove rax, rbx
                "
            )
        }
        Expr::Break(e) => {
            let e_is = compile_expr(e, si, env, brake, l);
            format!(
                "
              {e_is}
              jmp {brake}
            "
            )
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let e_is = compile_expr(e, si, env, &endloop, l);
            format!(
                "
                ; Loop {e:?}
              {startloop}:
              {e_is}
              jmp {startloop}
              {endloop}:
            "
            )
        }
        Expr::Block(es) => es
            .into_iter()
            .map(|e| compile_expr(e, si, env, brake, l))
            .collect::<Vec<String>>()
            .join("\n"),
        Expr::Lt(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let offset = si * 8;
            format!(
                "
                {e1_instrs}
                mov [rsp + {offset}], rax
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp + {offset}]
                test rbx, 1
                mov rbx, 7
                jne throw_error
                cmp rax, [rsp + {offset}]
                mov rbx, {TRUE_VAL}
                mov rax, {FALSE_VAL}
                cmovg rax, rbx
            "
            )
        }
        Expr::Gt(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let offset = si * 8;
            format!(
                "
                {e1_instrs}
                mov [rsp + {offset}], rax
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp + {offset}]
                test rbx, 1
                mov rbx, 7
                jne throw_error
                cmp rax, [rsp + {offset}]
                mov rbx, {TRUE_VAL}
                mov rax, {FALSE_VAL}
                cmovl rax, rbx
            "
            )
        }

        Expr::Eq(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let offset = si * 8;
            format!(
                "
                {e1_instrs}
                mov [rsp + {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp + {offset}]
                test rbx, 1
                mov rbx, 7
                jne throw_error
                cmp rax, [rsp + {offset}]
                mov rbx, {TRUE_VAL}
                mov rax, {FALSE_VAL}
                cmove rax, rbx
            "
            )
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_instrs = compile_expr(cond, si, env, brake, l);
            let thn_instrs = compile_expr(thn, si, env, brake, l);
            let els_instrs = compile_expr(els, si, env, brake, l);
            format!(
                "
                ; if block {e:?}
              {cond_instrs}
              cmp rax, {FALSE_VAL}
              je {else_label}
                {thn_instrs}
                jmp {end_label}
              {else_label}:
                {els_instrs}
              {end_label}:
           "
            )
        }
        Expr::Minus(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              mov rbx, 3
              jnz throw_error
              mov [rsp + {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              mov rbx, 3
              jnz throw_error
              sub rax, [rsp + {stack_offset}]
          "
            )
        }
        Expr::Plus(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              mov rbx, 3
              jnz throw_error
              mov [rsp + {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              mov rbx, 3
              jnz throw_error
              add rax, [rsp + {stack_offset}]
          "
            )
        },

        Expr::Let(name, val, body) => {
            let val_is = compile_expr(val, si, env, brake, l);
            let body_is = compile_expr(body, si + 1, &env.update(name.to_string(), si), brake, l);
            let offset = si * 8;
            format!(
                "
              {val_is}
              mov [rsp + {offset}], rax
              {body_is}
          "
            )
        }
        Expr::Call1(name, arg) => {
            let arg_is = compile_expr(arg, si, env, brake, l);
            let offset = 2 * 8; // one extra word for rdi saving, one for arg
            format!(
                "
                ; Calling {name:?} with {arg:?}
                {arg_is}
                sub rsp, {offset}
                mov [rsp], rax
                mov [rsp+8], rdi
                call {name}
                mov rdi, [rsp+8]
                add rsp, {offset}
            "
            )
        }
        Expr::Call2(name, arg1, arg2) => {
            let arg1_is = compile_expr(arg1, si, env, brake, l);
            let arg2_is = compile_expr(arg2, si + 1, env, brake, l);
            let curr_word = si * 8;
            let offset = 3 * 8;
            let curr_word_after_sub = offset + curr_word;
            // With this setup, the current word will be at [rsp+16], which is where arg1 is stored
            // We then want to get rdi at [rsp+16], arg2 at [rsp+8], and arg1 at [rsp], then call
            format!(
                "
                ; Calling {name} with {arg1:?}, {arg2:?}
                {arg1_is}
                mov [rsp + {curr_word}], rax
                {arg2_is}
                sub rsp, {offset}
                mov [rsp+16], rdi
                mov [rsp+8], rax
                
                mov rax, [rsp+{curr_word_after_sub}]
                mov [rsp], rax
                call {name}
                mov rdi, [rsp+16]
                add rsp, {offset}
            "
            )
        },

        Expr::Call3(name, arg1, arg2, arg3) => {
        let arg1_is = compile_expr(arg1, si, env, brake, l);
        let arg2_is = compile_expr(arg2, si + 1, env, brake, l);
        let arg3_is = compile_expr(arg3, si + 2, env, brake, l);
        let offset = 4 * 8;
        let arg1_location = si * 8;
        let arg2_location = (si + 1) * 8;
        let arg1_location_after_sub = offset + arg1_location;
        let arg2_location_after_sub = offset + arg2_location;
        

        // With this setup, the current word will be at [rsp+24], which is where arg1 is stored
        // We then want to get rdi at [rsp+24], arg3 at [rsp+16], arg2 at [rsp+8], and arg1 at [rsp], then call
        format!(
            "
            ; Calling {name} with {arg1:?}, {arg2:?}, {arg3:?}
            {arg1_is}
            mov [rsp + {arg1_location}], rax
            {arg2_is}
            mov [rsp + {arg2_location}], rax
            {arg3_is}
            sub rsp, {offset}
            mov [rsp+24], rdi
            mov [rsp+16], rax

            mov rax, [rsp+{arg2_location_after_sub}]
            mov [rsp + 8], rax

            mov rax, [rsp + {arg1_location_after_sub}]
            mov [rsp], rax

            call {name}
            mov rdi, [rsp+24]
            add rsp, {offset}
            "
            )
        }

        Expr::Tuple(exprs) => {
            let mut instrs = format!("; Making pair {exprs:?}");

            let expr_is = exprs
            .iter()
            .map(|(expr)|
            compile_expr(expr, si, env, brake, l));

            // We want to have this:
            // [r15]: exprs.len()
            // [r15 + 8]: exprs[0]
            // [r15 + 16]: exprs[1]
            // ...

            // We want to add the size of the array as a value to the beginning:
            // TODO: Does this need to be in the same value format???
            // TODO: I don't think so, but i'm not quite sure
            let size = exprs.len() as i64 * 2;
            instrs += format!("
            ; Push the size of the tuple to the first mem address
            mov rax, {size}
            mov [r15], rax
            ").as_str();

            for (i, e_is) in expr_is.enumerate() {
                
                // If we do the expr instructions
                instrs += &e_is.as_str();
                // Now rax has the last value in the array after everything
                let curr_word = 8 * (i + 1);
                // For a pair, this ^ does 
                // i = 0 | mov r15 + (8 * (2 - 0) = 16), rax=expr2
                // i = 1 | mov r15 + (8 * (2 - 1) = 8), rax=expr1

                // So we push the answer from rax to the heap
                instrs += format!("
                    mov [r15 + {curr_word}], rax
                ").as_str();

            }
            
            // Remember, we allocated 1 extra word for the len, so we have to account for that.
            let mem_allocated = (size + 1) * 8;
            instrs += format!("
                mov rax, r15
                add rax, 1
                add r15, {mem_allocated}
            ").as_str();

            instrs
        }

        Expr::Index(tup, idx) => {
            let mut instrs = "; Compiling index\n".to_string();
            let tup_is = compile_expr(tup, si, env, brake, l);
            let tup_offset = si * 8;
            let idx_is = compile_expr(idx, si + 1, env, brake, l);

            // Think we'll need to use 2 registers here perhaps?
            // Should make sure $rbx is 0d out

            instrs += format!("
                {tup_is}
                sub rax, 1
                mov [rsp + {tup_offset}], rax
                {idx_is}
            ").as_str();           
            // So now [rsp + {tup_offset}] has the mem address of the tuple
            // and the evaluated idx is in rax

            // TODO: Type check for idx here
            // TODO: Check for out of bounds here.
            // Out of bounds happens when ([[rsp + tup_offset]] = len) < idx

            instrs += format!("
                ; [rsp + {tup_offset}] has the heap location for the tuple
                mov rbx, [rsp + {tup_offset}]
                
                ; [[rsp + {tup_offset}]] has the 0th value (which is len)
                mov rbx, [rbx]

                ; rbx < rax => 2len < 2idx => len < idx
                cmp rbx, rax
                mov rbx, 8
                jl throw_error

                ; rax should still have 2idx, to get a word value, we mul by 4.
                imul rax, 4
                
                ; [rsp + {tup_offset}] should have the heap location
                mov rbx, [rsp + {tup_offset}]
                mov rax, [rbx + rax]
                mov rbx, 0
            ").as_str();

            instrs
        }
        // I don't think we need to follow the pattern here of storing the result each time,
        // We can just put the thing in the place.
        Expr::Pair(e1, e2) => {
            let e1is = compile_expr(e1, si, env, brake, l);
            let e2is = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
                ; Making pair {e1:?}, {e2:?}
                {e1is}
                mov [rsp + {stack_offset}], rax
                {e2is}
                mov [r15+8], rax
                mov rax, [rsp + {stack_offset}]
                mov [r15], rax
                mov rax, r15
                add rax, 1
                add r15, 16
            "
            )
        }

        

        Expr::Fst(e) => {
            let eis = compile_expr(e, si, env, brake, l);
            format!(
                "
                {eis}
                mov rax, [rax-1]
            "
            )
        }

        Expr::Snd(e) => {
            let eis = compile_expr(e, si, env, brake, l);
            format!(
                "
                {eis}
                mov rax, [rax+7]
            "
            )
        }

        Expr::SetFst(e1, e2) => {
            let e1is = compile_expr(e1, si, env, brake, l);
            let e2is = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
                {e1is}
                mov [rsp + {stack_offset}], rax
                {e2is}
                mov rbx, [rsp + {stack_offset}]
                mov [rbx-1], rax
                mov rax, [rsp + {stack_offset}]
            "
            )
        }

        Expr::SetSnd(e1, e2) => {
            let e1is = compile_expr(e1, si, env, brake, l);
            let e2is = compile_expr(e2, si + 1, env, brake, l);
            let stack_offset = si * 8;
            format!(
                "
                {e1is}
                mov [rsp + {stack_offset}], rax
                {e2is}
                mov rbx, [rsp + {stack_offset}]
                mov [rbx+7], rax
                mov rax, [rsp + {stack_offset}]
            "
            )
        },
        Expr::Tuple(_) => todo!(),
        Expr::Index(_, _) => todo!(),
        
    }
}

// Generated by ChatGPT
fn depth(e: &Expr) -> i32 {
    match e {
        Expr::Num(_) => 0,
        Expr::True => 0,
        Expr::False => 0,
        Expr::Add1(expr) => depth(expr),
        Expr::Sub1(expr) => depth(expr),
        Expr::IsNum(expr) => depth(expr),
        Expr::Plus(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Minus(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Let(_, expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Id(_) => 0,
        Expr::Lt(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Gt(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Eq(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::If(expr1, expr2, expr3) => depth(expr1).max(depth(expr2)).max(depth(expr3)),
        Expr::Loop(expr) => depth(expr),
        Expr::Block(exprs) => exprs.iter().map(|expr| depth(expr)).max().unwrap_or(0),
        Expr::Break(expr) => depth(expr),
        Expr::Print(expr) => depth(expr),
        Expr::Set(_, expr) => depth(expr),
        Expr::Call1(_, expr) => depth(expr),
        Expr::Call2(_, expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Call3(_, expr1, expr2, expr3) => depth(expr1).max(depth(expr2) + 1).max(depth(expr3) + 2),
        Expr::Pair(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Fst(expr) => depth(expr),
        Expr::Snd(expr) => depth(expr),
        Expr::SetFst(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::SetSnd(expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Tuple(exprs) => {
            let mut max = 0;
            for (i, expr) in exprs.iter().enumerate() {
                let tmp = depth(expr);
                if max < tmp {
                    max = tmp;
                }
            }
            max
        },
        Expr::Index(tup, idx) => depth(tup).max(depth(idx)),
    }
}

fn compile_program(p: &Program) -> (String, String) {
    let mut labels: i32 = 0;
    let mut defs: String = String::new();
    for def in &p.defs[..] {
        defs.push_str(&compile_definition(&def, &mut labels));
    }
    let depth = depth(&p.main);
    let offset = depth * 8;
    let main = compile_expr(&p.main, 0, &HashMap::new(), &String::from(""), &mut labels);
    let main_with_offsetting = format!(
        "
        sub rsp, {offset}
        {main}
        add rsp, {offset}
    "
    );
    (defs, main_with_offsetting)
}

fn compile_definition(d: &Definition, labels: &mut i32) -> String {
    match d {
        Fun1(name, arg, body) => {
            let depth = depth(body);
            let offset = depth * 8;
            let body_env = hashmap! {
                arg.to_string() => depth + 1
            };
            let body_is = compile_expr(body, 0, &body_env, &String::from(""), labels);
            format!(
                "{name}:
                sub rsp, {offset}
                {body_is}
                add rsp, {offset}
                ret
            "
            )
        }
        Fun2(name, arg1, arg2, body) => {
            let depth = depth(body);
            let offset = depth * 8;
            let body_env = hashmap! {
                arg1.to_string() => depth + 1,
                arg2.to_string() => depth + 2
            };
            let body_is = compile_expr(body, 0, &body_env, &String::from(""), labels);
            format!(
                "{name}:
                sub rsp, {offset}
                {body_is}
                add rsp, {offset}
                ret
                "
            )
        },
        Fun3(name, arg1, arg2, arg3, body) => {
            let depth = depth(body);
            let offset = depth * 8;
            let body_env = hashmap! {
                arg1.to_string() => depth + 1,
                arg2.to_string() => depth + 2,
                arg3.to_string() => depth + 3
            };
            let body_is = compile_expr(body, 0, &body_env, &String::from(""), labels);
            format!(
                "{name}:
                sub rsp, {offset}
                {body_is}
                add rsp, {offset}
                ret
                "
            )
        },
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let prog = "(".to_owned() + &in_contents + ")";

    let prog = parse_program(&parse(&prog).unwrap());
    let (defs, main) = compile_program(&prog);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
throw_error:
  push rsp
  mov rdi, rbx
  call snek_error
  ret
{}
our_code_starts_here:
  mov r15, rsi
  {}
  ret
",
        defs, main
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

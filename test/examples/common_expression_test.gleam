// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/option.{None, Some}
import gleam/set
import gleeunit/should

import nibble.{type Parser}
import nibble/lexer
import nibble/pratt

// LEXER TYPES -----------------------------------------------------------------------

type TokenT {
  PlusT
  MinusT
  StarT
  SlashT
  PercentT

  ExclamationMarkT
  QuestionMarkT
  ColonT
  CommaT

  LeftSquareT
  RightSquareT
  LeftParenT
  RightParenT

  LessThanEqT
  LessThanT
  GreaterThanEqT
  GreaterThanT
  EqualsT
  NotEqualsT
  InT

  AndT
  OrT

  IntT(Int)
  FloatT(Float)
  StringT(String)
  BoolT(Bool)
  NullT
  IdentT(String)
}

// PARSER TYPES -----------------------------------------------------------------------

pub type ArithmeticOp {
  Plus
  Minus
  Times
  Slash
  Percent
}

pub type RelationOp {
  LessThan
  LessThanEq
  GreaterThan
  GreaterThanEq
  Equals
  NotEquals
  In
}

pub type LogicalOp {
  And
  Or
}

pub type UnaryOp {
  Not
  UnaryMinus
}

pub type Atom {
  Int(Int)
  Float(Float)
  String(String)
  Bool(Bool)
  Null
}

pub type Expression {
  Arithmetic(Expression, ArithmeticOp, Expression)
  Relation(Expression, RelationOp, Expression)
  Logical(Expression, LogicalOp, Expression)
  Unary(UnaryOp, Expression)

  TernaryFork(Expression, Expression)
  Ternary(Expression, Expression, Expression)

  List(List(Expression))

  Atom(Atom)
  Ident(String)
}

type Context {
  InList
  InSubExpr
}

// LEXER -----------------------------------------------------------------------

fn lexer() {
  lexer.simple([
    lexer.identifier("[_a-zA-Z]", "[_a-zA-Z0-9]", set.new(), IdentT),
    lexer.token("+", PlusT),
    lexer.token("-", MinusT),
    lexer.token("*", StarT),
    lexer.token("/", SlashT),
    lexer.token("%", PercentT),
    lexer.token("[", LeftSquareT),
    lexer.token("]", RightSquareT),
    lexer.token("(", LeftParenT),
    lexer.token(")", RightParenT),
    lexer.token("<=", LessThanEqT),
    lexer.token("<", LessThanT),
    lexer.token(">=", GreaterThanEqT),
    lexer.token(">", GreaterThanT),
    lexer.token("==", EqualsT),
    lexer.token("!=", NotEqualsT),
    lexer.token("in", InT),
    lexer.token("&&", AndT),
    lexer.token("||", OrT),
    lexer.token("!", ExclamationMarkT),
    lexer.token("?", QuestionMarkT),
    lexer.token(":", ColonT),
    lexer.token(",", CommaT),
    lexer.number(IntT, FloatT),
    lexer.string("'", StringT),
    lexer.string("\"", StringT),
    lexer.token("true", BoolT(True)),
    lexer.token("false", BoolT(False)),
    lexer.whitespace(Nil)
      |> lexer.ignore,
  ])
}

// PARSER -----------------------------------------------------------------------

fn expression_parser() -> Parser(Expression, TokenT, Context) {
  let add = fn(lhs, rhs) { Arithmetic(lhs, Plus, rhs) }
  let sub = fn(lhs, rhs) { Arithmetic(lhs, Minus, rhs) }
  let mul = fn(lhs, rhs) { Arithmetic(lhs, Times, rhs) }
  let div = fn(lhs, rhs) { Arithmetic(lhs, Slash, rhs) }
  let mod = fn(lhs, rhs) { Arithmetic(lhs, Percent, rhs) }

  let lte = fn(lhs, rhs) { Relation(lhs, LessThanEq, rhs) }
  let lt = fn(lhs, rhs) { Relation(lhs, LessThan, rhs) }
  let gte = fn(lhs, rhs) { Relation(lhs, GreaterThanEq, rhs) }
  let gt = fn(lhs, rhs) { Relation(lhs, GreaterThan, rhs) }
  let eq = fn(lhs, rhs) { Relation(lhs, Equals, rhs) }
  let neq = fn(lhs, rhs) { Relation(lhs, NotEquals, rhs) }
  let in = fn(lhs, rhs) { Relation(lhs, In, rhs) }

  let and = fn(lhs, rhs) { Logical(lhs, And, rhs) }
  let or = fn(lhs, rhs) { Logical(lhs, Or, rhs) }

  let not = fn(expr) { Unary(Not, expr) }
  let unary_sub = fn(expr) { Unary(UnaryMinus, expr) }

  let ternary_fork = fn(then, otherwise) { TernaryFork(then, otherwise) }
  let ternary_cond = fn(left, right) {
    case right {
      TernaryFork(then, TernaryFork(fork_then, fork_otherwise)) ->
        TernaryFork(Ternary(left, then, fork_then), fork_otherwise)
      TernaryFork(then, otherwise) -> Ternary(left, then, otherwise)
      other -> other
    }
  }

  pratt.expression(
    one_of: [
      atom_parser,
      ident_parser,
      parens_parser,
      list_parser,
      pratt.prefix(8, nibble.token(ExclamationMarkT), not),
      pratt.prefix(8, nibble.token(MinusT), unary_sub),
    ],
    and_then: [
      pratt.infix_left(7, nibble.token(StarT), mul),
      pratt.infix_left(7, nibble.token(SlashT), div),
      pratt.infix_left(7, nibble.token(PercentT), mod),
      pratt.infix_left(6, nibble.token(PlusT), add),
      pratt.infix_left(6, nibble.token(MinusT), sub),
      pratt.infix_left(5, nibble.token(LessThanEqT), lte),
      pratt.infix_left(5, nibble.token(LessThanT), lt),
      pratt.infix_left(5, nibble.token(GreaterThanEqT), gte),
      pratt.infix_left(5, nibble.token(GreaterThanT), gt),
      pratt.infix_left(5, nibble.token(EqualsT), eq),
      pratt.infix_left(5, nibble.token(NotEqualsT), neq),
      pratt.infix_left(5, nibble.token(InT), in),
      pratt.infix_left(4, nibble.token(AndT), and),
      pratt.infix_left(3, nibble.token(OrT), or),
      pratt.infix_right(2, nibble.token(ColonT), ternary_fork),
      pratt.infix_right(1, nibble.token(QuestionMarkT), ternary_cond),
    ],
    dropping: nibble.succeed(Nil),
  )
}

fn list_parser(_) -> Parser(Expression, TokenT, Context) {
  use _ <- nibble.do(nibble.token(LeftSquareT))
  use exprs <- nibble.do_in(
    InList,
    nibble.sequence(expression_parser(), nibble.token(CommaT)),
  )
  use _ <- nibble.do(nibble.token(RightSquareT))

  nibble.return(List(exprs))
}

fn parens_parser(_) -> Parser(Expression, TokenT, Context) {
  use _ <- nibble.do(nibble.token(LeftParenT))
  use n <- nibble.do_in(InSubExpr, nibble.lazy(expression_parser))
  use _ <- nibble.do(nibble.token(RightParenT))

  nibble.return(n)
}

fn ident_parser(_) -> Parser(Expression, TokenT, Context) {
  use tok <- nibble.take_map("IDENT")

  case tok {
    IdentT(s) -> Ident(s) |> Some
    _ -> None
  }
}

fn atom_parser(_) -> Parser(Expression, TokenT, Context) {
  use tok <- nibble.take_map("STRING | INT | FLOAT | BOOL | NULL | IDENT")

  case tok {
    StringT(s) -> String(s) |> Atom |> Some
    IntT(n) -> Int(n) |> Atom |> Some
    FloatT(n) -> Float(n) |> Atom |> Some
    BoolT(b) -> Bool(b) |> Atom |> Some
    NullT -> Null |> Atom |> Some
    _ -> None
  }
}

// LITERAL TESTS ---------------------------------------------------------------

pub fn ternary_test() {
  let source = "a + 3 == 5 ? 'yes' : 'no'"

  let assert Ok(tokens) = lexer.run(source, lexer())
  let assert Ok(expr) = nibble.run(tokens, expression_parser())

  let expected =
    Ternary(
      Relation(Arithmetic(Ident("a"), Plus, Atom(Int(3))), Equals, Atom(Int(5))),
      Atom(String("yes")),
      Atom(String("no")),
    )

  expr |> should.equal(expected)
}

pub fn arithmetic_test() {
  let source = "a + 3 * (b - 7.3) / 8 + 6.8"

  let assert Ok(tokens) = lexer.run(source, lexer())
  let assert Ok(expr) = nibble.run(tokens, expression_parser())

  let expected =
    Arithmetic(
      Arithmetic(
        Ident("a"),
        Plus,
        Arithmetic(
          Arithmetic(
            Atom(Int(3)),
            Times,
            Arithmetic(Ident("b"), Minus, Atom(Float(7.3))),
          ),
          Slash,
          Atom(Int(8)),
        ),
      ),
      Plus,
      Atom(Float(6.8)),
    )

  expr |> should.equal(expected)
}

pub fn relational_list_test() {
  // FIXME: Lexer fails on '<=', '>=' and '!=' (and maybe more?)
  let source = "[a >= 3, 'wibble' != b, false || true && true == false]"

  let assert Ok(tokens) = lexer.run(source, lexer()) |> io.debug
  let assert Ok(expr) = nibble.run(tokens, expression_parser()) |> io.debug

  let expected =
    List([
      Relation(Ident("a"), GreaterThanEq, Atom(Int(3))),
      Relation(Atom(String("wibble")), NotEquals, Ident("b")),
      Logical(Ident("false"), Or, Logical(Ident("true"), And, Ident("true"))),
    ])

  expr |> io.debug |> should.equal(expected)
}

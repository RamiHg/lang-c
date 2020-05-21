extern crate peg;

peg::parser!( grammar langc(env: &Env) for str {

use self::peg::ParseLiteral;

use ast::*;
use astutil::*;
use env::{Env, Symbol};
use span::{Node, Span};

////
// Prologue
////

rule box<T>(ex: rule<T>) -> Box<T> = e:ex() { Box::new(e) }

rule node<T>(ex: rule<T>) -> Node<T> = l:position!() e:ex() r:position!() { Node::new(e, Span::span(l, r)) }

// Lists of elements.
rule list0<T>(ex: rule<T>) -> Vec<T> = e:ex() ** _ { e }
rule list1<T>(ex: rule<T>) -> Vec<T> = e:ex() ++ _ { e }
rule cs0<T>(ex: rule<T>) -> Vec<T> = e:ex() ** (_ "," _) { e }
rule cs1<T>(ex: rule<T>) -> Vec<T> = e:ex() ++ (_ "," _) { e }

// A list containing 0+ before's, 1 single, and 0+ after's.
rule list_010<T>(before: rule<T>, single: rule<T>, after: rule<T>) -> Vec<T> =
    before:list0(<before()>) _ single:single() _ after:list0(<after()>) {
        let mut before = before;
        before.push(single);
        before.extend(after);
        before
    }
// A list containing *exactly* one element of a, and any of b.
rule list_eq1_n<T>(a: rule<T>, b: rule<T>) -> Vec<T> = list_010(<b()>, <a()>, <b()>)
// A list containing *at least* one element of a, and any of b.
rule list_ge1_n<T>(a: rule<T>, b: rule<T>) -> Vec<T> = list_010(<b()>, <a()>, <a() / b()>)

////
// Whitespace
////

rule _() = quiet!{(['\r']?"\n"  directive()? / [' ' | '\t'])*}

rule directive() = "#" (!['\r' | '\n'][_])*

////
// 6.4.1 Keywords
////

rule alnum() = ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']

rule K<T>(e: rule<T>) -> T = quiet!{e:e() !alnum() { e }}
rule Ks(e: &'static str) -> &'static str = quiet!{##parse_string_literal(e) !alnum() { e }} / expected!("Keyword")

////
// 6.4.2 Identifiers
////

// Identifiers.
rule identifier() -> Node<Identifier> = node(<identifier0()>) / expected!("Identifier")

rule identifier0() -> Identifier =
    n:$(['_' | 'a'..='z' | 'A'..='Z'] ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) {?
        if !env.reserved.contains(n) {
            Ok(Identifier {
                name: n.into(),
            })
        } else {
            Err("identifier")
        }
    }

////
// 6.4.3 Universal character names
////

// TODO

////
// 6.4.4 Constants
////

rule ohx() = "0" ['x'|'X']
rule dec() = ['0'..='9']
rule oct() = ['0'..='7']
rule hex() = ['0'..='9' | 'a'..='f' | 'A'..='F']

pub rule constant() -> Constant =
    &['0'..='9' | '.'] c:numeric_constant(){ c } /
    &['\'' | 'u' | 'U' | 'L'] c:character_constant(){ Constant::Character(c) }

rule numeric_constant() -> Constant =
    c:float_constant(){ Constant::Float(c) } /
    c:integer_constant(){ Constant::Integer(c) }

rule integer_constant() -> Integer =
    n:integer_number() suffix:integer_suffix(){
        let (base, number) = n;
        Integer {
            base: base,
            number: number.to_owned().into_boxed_str(),
            suffix: suffix,
        }
    }

rule integer_number() -> (IntegerBase, &'input str) =
    n:$(['1'..='9'] dec()*) { (IntegerBase::Decimal, n) } /
    ohx() n:$(hex()+) { (IntegerBase::Hexadecimal, n) } /
    "0" n:$(oct()+) { (IntegerBase::Octal, n) } /
    n:$("0") { (IntegerBase::Decimal, n) }

rule integer_suffix() -> IntegerSuffix =
    quiet!{integer_suffix_inner()} / expected!("integer suffix")

rule integer_suffix_inner() -> IntegerSuffix =
    s:$((['u'|'U'|'l'|'L'] / gnu(<['i'|'I'|'j'|'J']>))*) {? int_suffix(s) }

rule float_constant() -> Float =
    n:float_number()suffix:float_suffix(){
        let (base, number) = n;
        Float {
            base: base,
            number: number.to_string().into_boxed_str(),
            suffix: suffix,
        }
    }

rule float_number() -> (FloatBase, &'input str) =
    n:$(float_decimal()) { (FloatBase::Decimal, n) } /
    ohx() n:$(float_hexadecimal()) { (FloatBase::Hexadecimal, n) }

rule float_decimal() =
    dec()* "." dec()+ float_decimal_exp()? /
    dec()+ "." float_decimal_exp()? /
    dec()+ float_decimal_exp()

rule float_decimal_exp() = ['e' | 'E']['+' | '-']?dec()+

rule float_hexadecimal() =
    hex()* "." hex()+ float_binary_exp() /
    hex()+ "." float_binary_exp() /
    hex()+ float_binary_exp()

rule float_binary_exp() = ['p' | 'P']['+' | '-']?dec()+

rule float_suffix() -> FloatSuffix = quiet!{float_suffix_inner()} / expected!("float literal suffix")

rule float_suffix_inner() -> FloatSuffix =
    gnu(<['i' | 'I' | 'j' | 'J']>) fmt:float_format(){
        FloatSuffix {
            format: fmt,
            imaginary: true,
        }
    } /
    fmt:float_format() imag:gnu(<['i' | 'I' | 'j' | 'J']>)? {
        FloatSuffix {
            format: fmt,
            imaginary: imag.is_some(),
        }
    }

rule float_format() -> FloatFormat =
    f:ts18661_float_suffix() { FloatFormat::TS18661Format(f) } /
    ['f'|'F'] { FloatFormat::Float } /
    ['l'|'L'] { FloatFormat::LongDouble } /
    { FloatFormat::Double }

rule character_constant() -> String =
    c:$(['L'|'u'|'U']? "'" character()+ "'") { String::from(c) }

rule character() = (!['\'' | '\\' | '\n'][_]) / escape_sequence()

rule escape_sequence() = "\\" (['\'' | '"' | '?' | '\\' | 'a' | 'b' | 'c' | 'f' | 'n' | 'r' | 't' | 'v'] / oct()*<1,3> / "x" hex()+)

////
// 6.4.5 String literal
////

pub rule string_literal() -> Node<Vec<String>> = s:node(<list1(<string_literal0()>)>) { s }

rule string_literal0() -> String =
    s:$(encoding_prefix()? "\"" string_char()* "\"") { String::from(s) }

rule encoding_prefix() = "u8" / ['u'|'U'|'L']

rule string_char() = (!['"' | '\\' | '\n'][_]) / escape_sequence()

////
// 6.5.1 Primary expression
////

rule primary_expression() -> Box<Node<Expression>> = box(<node(<primary_expression0()>)>)

rule primary_expression0() -> Expression =
    a:identifier(){ Expression::Identifier(Box::new(a)) } /
    a:node(<constant()>) { Expression::Constant(Box::new(a)) } /
    a:string_literal(){ Expression::StringLiteral(Box::new(a)) } /
    "(" _ a:expression0() _ ")" { a } /
    a:node(<generic_selection()>) { Expression::GenericSelection(Box::new(a)) } /
    gnu(<gnu_primary_expression()>)

rule generic_selection() -> GenericSelection =
    Ks("_Generic") _ "(" _ e:assignment_expression() _ "," _ a:cs1(<node(<generic_association()>)>) _ ")" {
        GenericSelection {
            expression: e,
            associations: a,
        }
    }

rule generic_association() -> GenericAssociation =
    t:type_name() _ ":" _ e:assignment_expression(){
        let span = Span::span(t.span.start, e.span.end);
        GenericAssociation::Type(Node::new(GenericAssociationType {
            type_name: t,
            expression: e,
        }, span))
    } /
    Ks("default") _ ":" _ e:assignment_expression(){
        GenericAssociation::Default(e)
    }

//// 6.5.2 Postfix operators

// rule postfix_expression() -> Box<Node<Expression>> = box(<node(<postfix_expression0()>)>)

rule postfix_expression0() -> Expression =
    e:node(<postfix_expression1()>) _ t:list0(<node(<postfix_expressionT()>)>) { apply_ops(t, e).node }

rule postfix_expression1() -> Expression =
    primary_expression0() /
    compound_literal()

rule postfix_expressionT() -> Operation =
    index_operator() /
    "(" _ e:cs0(<node(<assignment_expression0()>)>) _ ")" { Operation::Call(e) } /
    o:node(<member_operator()>) _ i:identifier(){ Operation::Member(o, i) } /
    o:node(<postfix_operator()>) { Operation::Unary(o) }

rule index_operator() -> Operation =
    i:node(<index_operator0()>) { Operation::Binary(Node::new(BinaryOperator::Index, i.span), i.node) }

rule index_operator0() -> Node<Expression> =
    "[" _ e:node(<expression0()>) _ "]" { e }

rule member_operator() -> MemberOperator =
    "." { MemberOperator::Direct } /
    "->" { MemberOperator::Indirect }

rule postfix_operator() -> UnaryOperator =
    "++" { UnaryOperator::PostIncrement } /
    "--" { UnaryOperator::PostDecrement }

rule compound_literal() -> Expression =
    n:node(<compound_literal_inner()>) { Expression::CompoundLiteral(Box::new(n)) }

rule compound_literal_inner() -> CompoundLiteral =
    "(" _ t:type_name() _ ")" _ "{" _ i:cs1(<node(<initializer()>)>) _ ","? _ "}" {
        CompoundLiteral {
            type_name: t,
            initializer_list: i,
        }
    }

////
// 6.5.3 Unary operators
////

rule unary_expression() -> Box<Node<Expression>> = box(<node(<unary_expression0()>)>)

rule unary_expression0() -> Expression =
    postfix_expression0() /
    unary_prefix() /
    unary_cast() /
    sizeof_expression() /
    alignof_expression() /
    gnu(<Ks("__extension__")>) _ e:unary_expression0(){ e }

rule unary_prefix() -> Expression =
    n:node(<unary_prefix_inner()>) { Expression::UnaryOperator(Box::new(n)) }

rule unary_prefix_inner() -> UnaryOperatorExpression =
    op:node(<prefix_operator()>) _ e:unary_expression(){
        UnaryOperatorExpression {
            operator: op,
            operand: e,
        }
    }

rule prefix_operator() -> UnaryOperator =
    "++" { UnaryOperator::PreIncrement } /
    "--" { UnaryOperator::PreDecrement } /
    Ks("sizeof") { UnaryOperator::SizeOf }

rule unary_cast() -> Expression =
    n:node(<unary_cast_inner()>) { Expression::UnaryOperator(Box::new(n)) }

rule unary_cast_inner() -> UnaryOperatorExpression =
    op:node(<unary_operator()>) _ e:cast_expression(){
        UnaryOperatorExpression {
            operator: op,
            operand: e,
        }
    }

rule unary_operator() -> UnaryOperator =
    "&"!"&" { UnaryOperator::Address } /
    "*" { UnaryOperator::Indirection } /
    "+" { UnaryOperator::Plus } /
    "-" { UnaryOperator::Minus } /
    "~" { UnaryOperator::Complement } /
    "!" { UnaryOperator::Negate }

rule sizeof_expression() -> Expression =
    Ks("sizeof") _ "(" _ t:type_name() _ ")" {
        Expression::SizeOf(Box::new(t))
    }

rule alignof_expression() -> Expression =
    (Ks("_Alignof") / gnu(<"__alignof" "__"?>)) _ "(" _ t:type_name() _ ")" {
        Expression::AlignOf(Box::new(t))
    }

////
// 6.5.4 Cast expressions
////

rule cast_expression() -> Box<Node<Expression>> = box(<node(<cast_expression0()>)>)

rule cast_expression0() -> Expression =
    c:node(<cast_expression_inner()>) { Expression::Cast(Box::new(c)) } /
    unary_expression0()


rule cast_expression_inner() -> CastExpression =
    "(" _ t:type_name() _ ")" _ e:cast_expression(){
        CastExpression {
            type_name: t,
            expression: e,
        }
    }

////
// 6.5.5 -- 6.5.14 Binary operators
////

rule rel_op() -> BinaryOperator =
    "<=" { BinaryOperator::LessOrEqual } /
    ">=" { BinaryOperator::GreaterOrEqual } /
    "<" { BinaryOperator::Less } /
    ">" { BinaryOperator::Greater }

rule mul_div_mod_op() -> BinaryOperator =
    "*" { BinaryOperator::Multiply } /
    "/" { BinaryOperator::Divide } /
    "%" { BinaryOperator::Modulo }

rule plus_minus_op() -> BinaryOperator =
    "+" { BinaryOperator::Plus } /
    "-" { BinaryOperator::Minus }

rule binary_expression() -> Box<Node<Expression>> = box(<binary_expression0()>)

rule binary_expression0() -> Node<Expression> = precedence! {
  x:(@) o:infix(<"||">) y:@ { infix(o, BinaryOperator::LogicalOr, x, y) }
--
  x:(@) o:infix(<"&&">) y:@ { infix(o, BinaryOperator::LogicalAnd, x, y) }
--
  x:(@) o:infix(<"|">) y:@ { infix(o, BinaryOperator::BitwiseOr, x, y) }
--
  x:(@) o:infix(<"^">) y:@ { infix(o, BinaryOperator::BitwiseXor, x, y) }
--
  x:(@) o:infix(<"&"!"&">) y:@ { infix(o, BinaryOperator::BitwiseAnd, x, y) }
--
    x:(@) o:infix(<"==">) y:@ { infix(o, BinaryOperator::Equals, x, y) }
    x:(@) o:infix(<"!=">) y:@ { infix(o, BinaryOperator::NotEquals, x, y) }
--
    x:(@) o:infix(<"<">) y:@ { infix(o, BinaryOperator::Less, x, y) }
    x:(@) o:infix(<">">) y:@ { infix(o, BinaryOperator::Greater, x, y) }
    x:(@) o:infix(<"<=">) y:@ { infix(o, BinaryOperator::LessOrEqual, x, y) }
    x:(@) o:infix(<">=">) y:@ { infix(o, BinaryOperator::GreaterOrEqual, x, y) }
--
  x:(@) o:infix(<"<<">) y:@ { infix(o, BinaryOperator::ShiftLeft, x, y) }
    x:(@) o:infix(<">>">) y:@ { infix(o, BinaryOperator::ShiftRight, x, y) }
--
    x:(@) _ o:node(<plus_minus_op()>) _ y:@ { infix2(o, x, y) }
--
    x:(@) _ o:node(<mul_div_mod_op()>) _ y:@ { infix2(o, x, y) }
--
  e:binary_operand() { e }
}

rule infix<T>(ex: rule<T>) -> Node<T> = _ n:node(<ex()>) _ { n }

rule binary_operand() -> Node<Expression> = node(<cast_expression0()>)

////
// 6.5.15 Conditional operator
////

rule conditional_expression() -> Box<Node<Expression>> = box(<node(<conditional_expression0()>)>)

rule conditional_expression0() -> Expression =
    a:binary_expression0() _ t:conditional_expressionT()? {
        if let Some((b, c)) = t {
            let span = Span::span(a.span.start, c.span.end);
            Expression::Conditional(Box::new(Node::new(ConditionalExpression {
                condition: Box::new(a),
                then_expression: b,
                else_expression: c,
            }, span)))
        } else {
            a.node
        }
    }

rule conditional_expressionT() -> (Box<Node<Expression>>, Box<Node<Expression>>) =
    "?" _ a:node(<expression0()>) _ ":" _ b:node(<conditional_expression0()>) { (Box::new(a), Box::new(b)) }

////
// 6.5.16 Assignment operators
////

rule assignment_expression() -> Box<Node<Expression>> = box(<node(<assignment_expression0()>)>)

rule assignment_expression0() -> Expression =

    // n:node(<assignment_expression_inner()>) { Expression::BinaryOperator(Box::new(n)) } /
    c:node(<conditional_expression0()>) _ a:assignment_expression_inner()? {
        if let Some((op, rhs)) = a {
            let span = Span::span(c.span.start, rhs.span.end);
            Expression::BinaryOperator(Box::new(
                Node::new(BinaryOperatorExpression {
                    operator: op,
                    lhs: Box::new(c),
                    rhs: rhs,
                }, span)
            ))
        } else {
            c.node
        }
    }

rule assignment_expression_inner() -> (Node<BinaryOperator>, Box<Node<Expression>>) =
    _ op:node(<assignment_operator()>) _ b:assignment_expression() {
        (op, b)
    }

rule assignment_operator() -> BinaryOperator =
    "=" { BinaryOperator::Assign } /
    "*=" { BinaryOperator::AssignMultiply } /
    "/=" { BinaryOperator::AssignDivide } /
    "%=" { BinaryOperator::AssignModulo } /
    "+=" { BinaryOperator::AssignPlus } /
    "-=" { BinaryOperator::AssignMinus } /
    "<<=" { BinaryOperator::AssignShiftLeft } /
    ">>=" { BinaryOperator::AssignShiftRight } /
    "&=" { BinaryOperator::AssignBitwiseAnd } /
    "^=" { BinaryOperator::AssignBitwiseXor } /
    "|=" { BinaryOperator::AssignBitwiseOr }

////
// 6.5.17 Comma operator
////

pub rule expression() -> Box<Node<Expression>> = box(<node(<expression0()>)>)

rule expression0() -> Expression =
    e:node(<assignment_expression0()>) _ t:list0(<expressionT()>) {
        if t.len() > 0 {
            let mut t  = t;
            t.insert(0, e);
            Expression::Comma(Box::new(t))
        } else {
            e.node
        }
    }

rule expressionT() -> Node<Expression> =
    "," _ e:node(<assignment_expression0()>) { e }

////
// 6.6 Constant expressions
////

rule constant_expression() -> Box<Node<Expression>> = conditional_expression()
rule constant_expression0() -> Expression = conditional_expression0()

////
// 6.7 Declarations
////

pub rule declaration() -> Node<Declaration> = node(<declaration0()>)

rule declaration0() -> Declaration =
    gnu(<Ks("__extension__")>)? _ d:declaration1() _ ";" {
        Declaration {
            specifiers: d.0,
            declarators: d.1,
        }
    }

rule declaration_seq(h: rule<Vec<Node<DeclarationSpecifier>>>, t: rule<(Vec<Node<DeclarationSpecifier>>, Vec<Node<InitDeclarator>>)>) -> (Vec<Node<DeclarationSpecifier>>, Vec<Node<InitDeclarator>>) =
    h:h() _ t:t(){ (concat(h, t.0), t.1) }

rule declaration1() -> (Vec<Node<DeclarationSpecifier>>, Vec<Node<InitDeclarator>>) =
    declaration_seq(<declaration_specifiers_unique()>, <declaration2()>)

rule declaration2() -> (Vec<Node<DeclarationSpecifier>>, Vec<Node<InitDeclarator>>) =
    declaration_seq(<declaration_typedef()>, <declaration_typedef_tail()>) /
    declaration_seq(<declaration_unique_type()>, <declaration_tail(<declaration_specifiers_unique()>)>) /
    declaration_seq(<declaration_nonunique_type()>, <declaration_tail(<declaration_specifiers_nonunique()>)>)

// What can follow a type specifier keyword or typename in a declaration
rule declaration_tail(s: rule<Vec<Node<DeclarationSpecifier>>>) -> (Vec<Node<DeclarationSpecifier>>, Vec<Node<InitDeclarator>>) =
    declaration_seq(<s()>, <declaration_tail1(<s()>)>)
rule declaration_tail1(s: rule<Vec<Node<DeclarationSpecifier>>>) -> (Vec<Node<DeclarationSpecifier>>, Vec<Node<InitDeclarator>>) =
    declaration_seq(<declaration_typedef()>, <declaration_typedef_tail1(<s()>)>) /
    d:declaration_init_declarators(){ (Vec::new(), d) }

// What can follow a typedef keyword
rule declaration_typedef_tail() -> (Vec<Node<DeclarationSpecifier>>, Vec<Node<InitDeclarator>>) =
    declaration_seq(<declaration_unique_type()>, <declaration_typedef_tail1(<declaration_specifiers_unique()>)>) /
    declaration_seq(<declaration_nonunique_type()>, <declaration_typedef_tail1(<declaration_specifiers_nonunique()>)>)

// What can follow after typedef + type name
rule declaration_typedef_tail1(s: rule<Vec<Node<DeclarationSpecifier>>>) -> (Vec<Node<DeclarationSpecifier>>, Vec<Node<InitDeclarator>>) = s:s() _ d:declaration_type_declarators(){ (s, d) }

rule declaration_unique_type() -> Vec<Node<DeclarationSpecifier>> =
    n:node(<declaration_specifier_unique_type0()>) { vec![ n ] }

rule declaration_nonunique_type() -> Vec<Node<DeclarationSpecifier>> =
    n:node(<declaration_specifier_nonunique_type0()>) { vec![ n ] }

rule declaration_specifiers() -> Vec<Node<DeclarationSpecifier>> =
    s:declaration_specifiers_unique() _ t:declaration_specifiers_tail(){ concat(s, t) }

rule declaration_specifiers_tail() -> Vec<Node<DeclarationSpecifier>> =
    t:declaration_unique_type() _ s:declaration_specifiers_unique(){ concat(t, s) } /
    t:declaration_nonunique_type() _ s:declaration_specifiers_nonunique(){ concat(t, s) }

rule declaration_specifiers_unique() -> Vec<Node<DeclarationSpecifier>> =
    list0(<node(<declaration_specifier_nontype()>)>)

rule declaration_specifiers_nonunique() -> Vec<Node<DeclarationSpecifier>> =
    list0(<node(<declaration_specifier_nontype() / declaration_specifier_nonunique_type0()>)>)

rule declaration_specifier_nontype() -> DeclarationSpecifier =
    s:storage_class_specifier() { DeclarationSpecifier::StorageClass(s) } /
    s:type_qualifier() { DeclarationSpecifier::TypeQualifier(s) } /
    s:function_specifier() { DeclarationSpecifier::Function(s) } /
    s:alignment_specifier() { DeclarationSpecifier::Alignment(s) } /
    s:gnu(<attribute_specifier()>) { DeclarationSpecifier::Extension(s) } /
    s:ms(<ms_extension_specifier_list()>) { DeclarationSpecifier::Extension(s) }

rule declaration_typedef() -> Vec<Node<DeclarationSpecifier>> =
    s:node(<declaration_typedef0()>) { vec![ s ] }

rule declaration_typedef0() -> DeclarationSpecifier =
    s:storage_class_typedef(){ DeclarationSpecifier::StorageClass(s) }

rule declaration_specifier_unique_type0() -> DeclarationSpecifier =
    s:node(<type_specifier_unique()>) { DeclarationSpecifier::TypeSpecifier(s) }

rule declaration_specifier_nonunique_type0() -> DeclarationSpecifier =
    s:node(<type_specifier_nonunique()>) { DeclarationSpecifier::TypeSpecifier(s) }

rule declaration_init_declarators() -> Vec<Node<InitDeclarator>> = cs0(<node(<init_declarator()>)>)

rule declaration_type_declarators() -> Vec<Node<InitDeclarator>> = cs0(<node(<type_declarator()>)>)

rule init_declarator() -> InitDeclarator =
    d:init_declarator_declarator() _ e:gnu(<init_declarator_gnu()>)? _ i:node(<init_declarator_init()>)?
    {
        InitDeclarator {
            declarator: with_ext(d, e),
            initializer: i,
        }
    }

rule init_declarator_declarator() -> Node<Declarator> =
    d:declarator(){
        env.handle_declarator(&d, Symbol::Identifier);
        d
    }

rule init_declarator_init() -> Initializer =
    "=" _ i:initializer(){ i }

rule init_declarator_gnu() -> Vec<Node<Extension>> =
    l:asm_label()? _ a:attribute_specifier_list(){ l.into_iter().chain(a).collect() }

rule type_declarator() -> InitDeclarator =
    d:declarator() _ e:gnu(<init_declarator_gnu()>)?
    {
        env.handle_declarator(&d, Symbol::Typename);
        InitDeclarator {
            declarator: with_ext(d, e),
            initializer: None,
        }
    }

////
// 6.7.1 Storage-class specifiers
////

rule storage_class_specifier() -> Node<StorageClassSpecifier> = node(<storage_class_specifier0()>)

rule storage_class_specifier0() -> StorageClassSpecifier =
    Ks("extern") { StorageClassSpecifier::Extern } /
    Ks("static") { StorageClassSpecifier::Static } /
    Ks("_Thread_local") { StorageClassSpecifier::ThreadLocal } /
    Ks("auto") { StorageClassSpecifier::Auto } /
    Ks("register") { StorageClassSpecifier::Register }

rule storage_class_typedef() -> Node<StorageClassSpecifier> = node(<storage_class_typedef0()>)

rule storage_class_typedef0() -> StorageClassSpecifier =
    Ks("typedef") { StorageClassSpecifier::Typedef }

////
// 6.7.2 Type specifiers
////

// ISO 2011, 6.7.2, §2. Void, _Bool, _Atomic, typedef names, struct/unions, and enum
// specifiers can only appear once in declaration specifiers or specifier-qualifiers.
// This resolves the ambiguity with typedef names.
rule type_specifier_unique() -> TypeSpecifier =
    Ks("void") { TypeSpecifier::Void } /
    Ks("_Bool") { TypeSpecifier::Bool } /
    Ks("_Atomic") _ "(" _ t:type_name() _ ")" { TypeSpecifier::Atomic(t) } /
    s:node(<struct_or_union_specifier()>) { TypeSpecifier::Struct(s) } /
    e:node(<enum_specifier()>) { TypeSpecifier::Enum(e) } /
    t:typedef_name(){ TypeSpecifier::TypedefName(t) }

rule type_specifier_nonunique() -> TypeSpecifier =
    Ks("char") { TypeSpecifier::Char } /
    Ks("short") { TypeSpecifier::Short } /
    Ks("int") { TypeSpecifier::Int } /
    Ks("long") { TypeSpecifier::Long } /
    Ks("float") { TypeSpecifier::Float } /
    Ks("double") { TypeSpecifier::Double } /
    K(<"signed" / gnu(<"__signed" "__"?>)>) { TypeSpecifier::Signed } /
    Ks("unsigned") { TypeSpecifier::Unsigned } /
    K(<"_Complex" / gnu(<"__complex" "__"?>)>) { TypeSpecifier::Complex } /
    t:K(<ts18661_float_type_specifier()>) { TypeSpecifier::TS18661Float(t) } /
    gnu(<typeof_specifier()>) /
    ms(<ms_integer_types()>)

rule struct_or_union_specifier() -> StructType =
    t:node(<struct_or_union()>) _ i:identifier()? _ d:struct_or_union_body(){
        StructType {
            kind: t,
            identifier: i,
            declarations: d,
        }
    } /
    t:node(<struct_or_union()>) _ i:identifier(){
        StructType {
            kind: t,
            identifier: Some(i),
            declarations: None,
        }
    }

rule struct_or_union_body() -> Option<Vec<Node<StructDeclaration>>> =
    "{" _ d:list1(<node(<struct_declaration()>)>) _ "}" { Some(d) } /
    gnu(<"{" _ "}">) { Some(Vec::new()) } /
    { None }

rule struct_or_union() -> StructKind =
    Ks("struct") { StructKind::Struct } /
    Ks("union") { StructKind::Union }

rule struct_declaration() -> StructDeclaration =
    f:node(<struct_field()>) { StructDeclaration::Field(f) } /
    s:static_assert(){ StructDeclaration::StaticAssert(s) } /
    gnu(<Ks("__extension__")>) _ d:struct_declaration(){ d }

rule struct_field() -> StructField =
    s:specifier_qualifiers() _ d:cs0(<node(<struct_declarator()>)>) _ ";" {
        StructField {
            specifiers: s,
            declarators: d,
        }
    }

rule specifier_qualifiers() -> Vec<Node<SpecifierQualifier>> =
    list_eq1_n(<node(<specifier_qualifier_unique_type0()>)>, <node(<specifier_qualifier_qualifier0()>)>) /
    list_ge1_n(<node(<specifier_qualifier_nonunique_type0()>)>, <node(<specifier_qualifier_qualifier0()>)>)

rule specifier_qualifier_unique_type0() -> SpecifierQualifier =
    s:node(<type_specifier_unique()>) { SpecifierQualifier::TypeSpecifier(s) }

rule specifier_qualifier_nonunique_type0() -> SpecifierQualifier =
    s:node(<type_specifier_nonunique()>) { SpecifierQualifier::TypeSpecifier(s) }

rule specifier_qualifier_qualifier0() -> SpecifierQualifier =
    q:type_qualifier(){ SpecifierQualifier::TypeQualifier(q) }

rule struct_declarator() -> StructDeclarator =
    d:declarator()? _ ":" _ e:constant_expression() a:gnu(<attribute_specifier_list()>)? {
        StructDeclarator {
            declarator: d.map(|d| with_ext(d, a)),
            bit_width: Some(e),
        }
    } /
    d:declarator() a:gnu(<attribute_specifier_list()>)? {
        StructDeclarator {
            declarator: Some(with_ext(d, a)),
            bit_width: None,
        }
    }

rule enum_specifier() -> EnumType =
    Ks("enum") _ i:identifier()? _ "{" _ e:cs1(<node(<enumerator()>)>) _ ","? _ "}" {
        EnumType {
            identifier: i,
            enumerators: e,
        }
    } /
    Ks("enum") _ i:identifier(){
        EnumType {
            identifier: Some(i),
            enumerators: Vec::new(),
        }
    }

rule enumerator() -> Enumerator =
    i:identifier() _ e:enumerator_constant()? {
        env.add_symbol(&i.node.name, Symbol::Identifier);
        Enumerator {
            identifier: i,
            expression: e,
        }
    }

rule enumerator_constant() -> Box<Node<Expression>> =
    "=" _ e:constant_expression(){ e }

////
// 6.7.3 Type qualifiers
////

rule type_qualifier() -> Node<TypeQualifier> = node(<type_qualifier0()>)

rule type_qualifier0() -> TypeQualifier =
    K(<"const"    / gnu(<"__const">)>) { TypeQualifier::Const } /
    K(<"restrict" / gnu(<"__restrict" "__"?>)>) { TypeQualifier::Restrict } /
    K(<"volatile" / gnu(<"__volatile" "__"?>)>) { TypeQualifier::Volatile } /
    clang(<Ks("_Nonnull")>) { TypeQualifier::Nonnull } /
    clang(<Ks("_Null_unspecified")>) { TypeQualifier::NullUnspecified } /
    clang(<Ks("_Nullable")>) { TypeQualifier::Nullable } /
    // 6.7.2.4: _Atomics followed by a "(" are interpreted as type specifiers.
    Ks("_Atomic") _ !"(" { TypeQualifier::Atomic }

////
// 6.7.4 Function specifiers
////

rule function_specifier() -> Node<FunctionSpecifier> = node(<function_specifier0()>)

rule function_specifier0() -> FunctionSpecifier =
    K(<"inline" / gnu(<"__inline" "__"?>)>) { FunctionSpecifier::Inline } /
    Ks("_Noreturn") { FunctionSpecifier::Noreturn }

////
// 6.7.5 Alignment specifiers
////

rule alignment_specifier() -> Node<AlignmentSpecifier> = node(<alignment_specifier0()>)

rule alignment_specifier0() -> AlignmentSpecifier =
    Ks("_Alignas") _ "(" _ t:type_name() _ ")" { AlignmentSpecifier::Type(t) } /
    Ks("_Alignas") _ "(" _ e:constant_expression() _ ")" { AlignmentSpecifier::Constant(e) }

////
// 6.7.6 Declarators
////

rule declarator() -> Node<Declarator> = node(<declarator0()>)

rule declarator0() -> Declarator =
    attr:gnu(<attribute_specifier_list()>)?
    pointer:list0(<pointer()>) _
    kind:node(<direct_declarator()>) _
    derived:list0(<node(<derived_declarator()>)>)
    {
        Declarator {
            kind: kind,
            derived: concat(pointer, derived),
            extensions: attr.unwrap_or_default(),
        }
    }

rule direct_declarator() -> DeclaratorKind =
    i:identifier(){ DeclaratorKind::Identifier(i) } /
    "(" _ d:declarator() _ ")" { DeclaratorKind::Declarator(Box::new(d)) }

rule derived_declarator() -> DerivedDeclarator =
    "[" _ a:node(<array_declarator()>) { DerivedDeclarator::Array(a) } /
    "(" _ f:scoped(<node(<function_declarator()>)>) _ ")" { DerivedDeclarator::Function(f) } /
    "(" _ p:cs0(<identifier()>) _ ")" { DerivedDeclarator::KRFunction(p) }

rule array_declarator() -> ArrayDeclarator =
    q:list0(<type_qualifier()>) _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::Unknown,
        }
    } /
    q:list0(<type_qualifier()>) _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::VariableExpression(e),
        }
    } /
    Ks("static") _ q:list0(<type_qualifier()>) _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::StaticExpression(e),
        }
    } /
    q:list1(<type_qualifier()>) _ Ks("static") _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::StaticExpression(e),
        }
    } /
    q:list0(<type_qualifier()>) _ "*" _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::VariableUnknown,
        }
    }

rule function_declarator() -> FunctionDeclarator =
    p:cs1(<parameter_declaration()>) _ e:ellipsis(){
        FunctionDeclarator {
            parameters: p,
            ellipsis: e,
        }
    }

rule pointer() -> Node<DerivedDeclarator> = node(<pointer0()>)

rule pointer0() -> DerivedDeclarator =
    "*" _ q:list0(<node(<pointer_qualifier()>)>) { DerivedDeclarator::Pointer(q) }

rule pointer_qualifier() -> PointerQualifier =
    q:type_qualifier(){ PointerQualifier::TypeQualifier(q) } /
    e:gnu(<attribute_specifier()>) { PointerQualifier::Extension(e) } /
    e:ms(<ms_extension_specifier_list()>) { PointerQualifier::Extension(e) }

rule ellipsis() -> Ellipsis =
    "," _ "..." { Ellipsis::Some } / { Ellipsis::None }

rule parameter_declaration() -> Node<ParameterDeclaration> = node(<parameter_declaration0()>)

rule parameter_declaration0() -> ParameterDeclaration =
    s:declaration_specifiers() _ d:parameter_declarator() _ a:gnu(<attribute_specifier_list()>)? {
        ParameterDeclaration {
            specifiers: s,
            declarator: d,
            extensions: a.unwrap_or_default()
        }
    }

rule parameter_declarator() -> Option<Node<Declarator>> =
    d:declarator(){
        env.handle_declarator(&d, Symbol::Identifier);
        Some(d)
    } /
    d:abstract_declarator(){ Some(d) } /
    { None }

////
// 6.7.7 Type names
////

rule type_name() -> Node<TypeName> = node(<type_name0()>)

rule type_name0() -> TypeName =
    s:specifier_qualifiers() _ d:abstract_declarator()? {
        TypeName {
            specifiers: s,
            declarator: d,
        }
    }

rule abstract_declarator() -> Node<Declarator> = node(<abstract_declarator0()>)

rule abstract_declarator0() -> Declarator =
    p:list0(<pointer()>) _ k:node(<direct_abstract_declarator()>) _ d:list0(<derived_abstract_declarator()>) {
        Declarator {
            kind: k,
            derived: concat(p, d),
            extensions: Vec::new(),
        }
    } /
    p:list0(<pointer()>) k:position!() _ d:list1(<derived_abstract_declarator()>) {
        Declarator {
            kind: Node::new(DeclaratorKind::Abstract, Span::span(k, k)),
            derived: concat(p, d),
            extensions: Vec::new(),
        }
    } /
    p:list1(<pointer()>) k:position!() {
        Declarator {
            kind: Node::new(DeclaratorKind::Abstract, Span::span(k, k)),
            derived: p,
            extensions: Vec::new(),
        }
    }

rule direct_abstract_declarator() -> DeclaratorKind =
    "(" _ d:abstract_declarator() _ ")" { DeclaratorKind::Declarator(Box::new(d)) }

rule derived_abstract_declarator() -> Node<DerivedDeclarator> = node(<derived_abstract_declarator0()>)

rule derived_abstract_declarator0() -> DerivedDeclarator =
    "[" _ a:node(<abstract_array_declarator()>) { DerivedDeclarator::Array(a) } /
    "(" _ d:node(<abstract_function_declarator()>) _ ")" { DerivedDeclarator::Function(d) }

rule abstract_array_declarator() -> ArrayDeclarator =
    q:list0(<type_qualifier()>) _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::Unknown,
        }
    } /
    q:list0(<type_qualifier()>) _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::VariableExpression(e),
        }
    } /
    Ks("static") _ q:list0(<type_qualifier()>) _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::StaticExpression(e),
        }
    } /
    q:list1(<type_qualifier()>) _ Ks("static") _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::StaticExpression(e),
        }
    } /
    "*" _ "]" {
        ArrayDeclarator {
            qualifiers: Vec::new(),
            size: ArraySize::VariableUnknown,
        }
    }

rule abstract_function_declarator() -> FunctionDeclarator =
    p:cs1(<parameter_declaration()>) _ e:ellipsis(){
        FunctionDeclarator {
            parameters: p,
            ellipsis: e,
        }
    } /
    {
        FunctionDeclarator {
            parameters: Vec::new(),
            ellipsis: Ellipsis::None,
        }
    }


////
// 6.7.8 Type definitions
////

rule typedef_name() -> Node<Identifier> = quiet!{typedef_name0()} / expected!("<typedef_name>")

rule typedef_name0() -> Node<Identifier> = i:identifier(){?
    if env.is_typename(&i.node.name) {
        Ok(i)
    } else {
        Err("<unused>")
    }
}

////
// 6.7.9 Initialization
////

rule initializer() -> Initializer =
    e:assignment_expression(){ Initializer::Expression(e) } /
    "{" _ i:cs1(<node(<initializer_list_item()>)>) _ ","? _ "}" { Initializer::List(i) } /
    gnu(<"{" _ "}">) { Initializer::List(Vec::new()) }

rule initializer_list_item() -> InitializerListItem =
    d:designation()? _ i:node(<initializer()>) {
        InitializerListItem {
            designation: d.unwrap_or_default(),
            initializer: Box::new(i),
        }
    }

rule designation() -> Vec<Node<Designator>> =
    d:list1(<node(<designator()>)>) _ "=" { d } /
    d:gnu(<node(<colon_designation()>)>) { vec! [ d ] } /
    d:gnu(<node(<array_designator()>)>) { vec![ d ] }

rule colon_designation() -> Designator =
    i:identifier() _ ":" { Designator::Member(i) }

rule designator() -> Designator =
    d:array_designator(){ d } /
    "." _ i:identifier(){ Designator::Member(i) }

rule array_designator() -> Designator =
    "[" _ a:node(<constant_expression0()>) _ b:gnu(<range_designator_ext()>)? "]" {
        match b {
            Some(b) => {
                let span = Span::span(a.span.start, b.span.end);
                Designator::Range(Node::new(RangeDesignator { from: a, to: b }, span))
            }
            None => Designator::Index(a),
        }
    }

rule range_designator_ext() -> Node<Expression> =
    "..." _ e:node(<constant_expression0()>) { e }

////
// 6.7.10 Static assertions
////

rule static_assert() -> Node<StaticAssert> = node(<static_assert0()>)

rule static_assert0() -> StaticAssert =
   gnu(<Ks("__extension__")>)?
   _ Ks("_Static_assert") _ "(" _ e:constant_expression() _ "," _ s:string_literal() _ ")" _ ";" {
        StaticAssert {
            expression: e,
            message: s,
        }
    }

////
// 6.8 Statements and blocks
////

pub rule statement() -> Box<Node<Statement>> = box(<node(<statement0()>)>)

rule statement0() -> Statement =
    s:node(<labeled_statement()>) { Statement::Labeled(s) } /
    scoped(<compound_statement()>) /
    expression_statement() /
    scoped(<selection_statement()>) /
    scoped(<iteration_statement()>) /
    jump_statement() /
    gnu(<asm_statement()>)

////
// 6.8.1 Labeled statements
////

rule labeled_statement() -> LabeledStatement =
    l:node(<label()>) _ ":" _ s:statement(){
        LabeledStatement {
            label: l,
            statement: s,
        }
    }

rule label() -> Label =
    i:identifier(){ Label::Identifier(i) } /
    Ks("case") _ e:constant_expression(){ Label::Case(e) } /
    Ks("default") { Label::Default }

////
// 6.8.2 Compound statement
////

rule compound_statement() -> Statement =
    "{" _ b:list0(<node(<block_item()>)>) _ "}" { Statement::Compound(b) }

rule block_item() -> BlockItem =
    d:declaration(){ BlockItem::Declaration(d) } /
    s:static_assert(){ BlockItem::StaticAssert(s) } /
    s:node(<statement0()>) { BlockItem::Statement(s) }

////
// 6.8.3 Expression and null statements
////

rule expression_statement() -> Statement =
    e:expression()? _ ";" { Statement::Expression(e) }

////
// 6.8.4 Selection statement
////

rule selection_statement() -> Statement =
    s:node(<if_statement()>) { Statement::If(s) } /
    s:node(<switch_statement()>) { Statement::Switch(s) }

rule if_statement() -> IfStatement =
    Ks("if") _ "(" _ e:expression() _ ")" _ a:statement() _ b:else_statement()? {
        IfStatement {
            condition: e,
            then_statement: a,
            else_statement: b,
        }
    }

rule else_statement() -> Box<Node<Statement>> = Ks("else") _ s:statement(){ s }

rule switch_statement() -> SwitchStatement =
    Ks("switch") _ "(" _ e:expression() _ ")" _ s:statement(){
        SwitchStatement {
            expression: e,
            statement: s,
        }
    }

////
// 6.8.5 Iteration statement
////

rule iteration_statement() -> Statement =
    s:node(<while_statement()>) { Statement::While(s) } /
    s:node(<do_while_statement()>) { Statement::DoWhile(s) } /
    s:node(<for_statement()>) { Statement::For(s) }

rule while_statement() -> WhileStatement =
    Ks("while") _ "(" _ e:expression() _ ")" _ s:statement(){
        WhileStatement {
            expression: e,
            statement: s,
        }
    }

rule do_while_statement() -> DoWhileStatement =
    Ks("do") _ s:statement() _ Ks("while") _ "(" _ e:expression() _ ")" _ ";" {
        DoWhileStatement {
            statement: s,
            expression: e,
        }
    }

rule for_statement() -> ForStatement =
    Ks("for") _ "(" _ a:node(<for_initializer()>) _ b:expression()? _ ";" _ c:expression()? _ ")" _ s:statement(){
        ForStatement {
            initializer: a,
            condition: b,
            step: c,
            statement: s,
        }
    }

rule for_initializer() -> ForInitializer =
    e:expression() _ ";" { ForInitializer::Expression(e) } /
    d:declaration() { ForInitializer::Declaration(d) } /
    s:static_assert() { ForInitializer::StaticAssert(s) } /
    ";" { ForInitializer::Empty }

////
// 6.8.6 Jump statements
////

rule jump_statement() -> Statement =
    Ks("goto") _ i:identifier() _ ";" { Statement::Goto(i) } /
    Ks("continue") _ ";" { Statement::Continue } /
    Ks("break") _ ";" { Statement::Break } /
    Ks("return") _ e:expression()? _ ";" { Statement::Return(e) }

////
// 6.9 External definitions
////

rule scoped<T>(e: rule<T>) -> T = ({ env.enter_scope(); }) e:e()? {? env.leave_scope(); e.ok_or("") }

pub rule translation_unit() -> TranslationUnit =
    directive()? _ d:list0(<node(<external_declaration()>)>) _ { TranslationUnit(d) }

rule external_declaration() -> ExternalDeclaration =
    d:declaration() { ExternalDeclaration::Declaration(d) } /
    s:static_assert() { ExternalDeclaration::StaticAssert(s) } /
    d:scoped(<node(<function_definition()>)>) { ExternalDeclaration::FunctionDefinition(d) }

rule function_definition() -> FunctionDefinition =
    gnu(<Ks("__extension__")>)?
    _ a:declaration_specifiers() _ b:declarator() _ c:list0(<declaration()>)
    _ d:node(<compound_statement()>) {
        FunctionDefinition {
            specifiers: a,
            declarator: b,
            declarations: c,
            statement: d,
        }
    }

////
// GNU extensions
////

rule gnu<T>(E: rule<T>) -> T = &gnu_guard() e:E() { e }

rule gnu_guard() = {? if env.extensions_gnu { Ok(()) } else { Err("gnu extensions disabled") } }

////
// GNU attributes
////

rule attribute_specifier_list() -> Vec<Node<Extension>> =
    a:list0(<attribute_specifier()>) { a.into_iter().flat_map(|v| v).collect() }

rule attribute_specifier() -> Vec<Node<Extension>> =
    Ks("__attribute__") _ "((" _ a:cs0(<node(<attribute()>)>) _ "))" { a }

rule attribute() -> Extension =
    c:clang(<node(<attr_availability()>)>) { Extension::AvailabilityAttribute(c) } /
    n:node(<attribute_name()>) _ p:attribute_parameters()? {
        Extension::Attribute(Attribute {
            name: n,
            arguments: p.unwrap_or_default(),
        })
    }

rule attribute_name() -> String =
    n:$(quiet!{['_' | 'a'..='z' | 'A'..='Z']['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*}) { String::from(n) }

rule attribute_parameters() -> Vec<Node<Expression>> =
    "(" _ e:cs0(<node(<assignment_expression0()>)>) _ ")" { e }

rule attr_availability() -> AvailabilityAttribute =
    Ks("availability") _ "(" _ p:identifier() _ "," _ c:cs1(<node(<attr_availability_clause()>)>) _ ")" {
        AvailabilityAttribute {
            platform: p,
            clauses: c,
        }
    }

rule attr_availability_clause() -> AvailabilityClause =
    Ks("introduced") _ "=" _ v:node(<attr_availability_version()>) { AvailabilityClause::Introduced(v) } /
    Ks("deprecated") _ "=" _ v:node(<attr_availability_version()>) { AvailabilityClause::Deprecated(v) } /
    Ks("obsoleted") _ "=" _ v:node(<attr_availability_version()>) { AvailabilityClause::Obsoleted(v) } /
    Ks("unavailable") { AvailabilityClause::Unavailable } /
    Ks("message") _ "=" _ s:string_literal(){ AvailabilityClause::Message(s) } /
    Ks("replacement") _ "=" _ s:string_literal(){ AvailabilityClause::Replacement(s) }

rule attr_availability_version() -> AvailabilityVersion =
    a:$(dec()+) b:$("." dec()+)? c:$("." dec()+)? {
        AvailabilityVersion {
            major: a.into(),
            minor: b.map(|b| b[1..].to_owned()),
            subminor: c.map(|c| c[1..].to_owned()),
        }
    }

////
// GNU assembler labels
////

rule asm_label() -> Node<Extension> = node(<asm_label0()>)

rule asm_label0() -> Extension =
    asm_label_keyword() _ "(" _ s:string_literal() _ ")" { Extension::AsmLabel(s) }

rule asm_label_keyword() =
    quiet!{Ks("asm") / K(<"__asm" "__"?>)} / expected!("asm")

////
// GNU assembler statements
////

rule asm_statement() -> Statement =
    s:node(<asm_statement0()>) { Statement::Asm(s) }

rule asm_statement0() -> AsmStatement =
    K(<"asm" / "__asm" "__"?>) _ q:type_qualifier()? _ "(" _
        a:string_literal() _
        o:asm_ext(<asm_operand_list()>, <asm_ext(<asm_operand_list()>, <asm_ext(<cs0(<string_literal()>)>, <()>)>)>)? _
    ")" _ ";" {
        if let Some((o, i)) = o {
            AsmStatement::GnuExtended(GnuExtendedAsmStatement {
                qualifier: q,
                template: a,
                outputs: o,
                inputs: i.clone().map(|i| i.0).unwrap_or_default(),
                clobbers: i.map(|i| i.1).unwrap_or_default().map(|i| i.0).unwrap_or_default(),
            })
        } else {
            AsmStatement::GnuBasic(a)
        }
    }

rule asm_ext<E, T>(e: rule<E>, t: rule<T>) -> (E, Option<T>) = ":" _ e:e() _ t:t()? { (e, t) }

rule asm_operand_list() -> Vec<Node<GnuAsmOperand>> = cs0(<node(<asm_operand()>)>)

rule asm_operand() -> GnuAsmOperand =
    i:("[" _ i:identifier() _ "]" _ {i})? s:string_literal() _ "(" _ e:node(<expression0()>) _ ")" {
        GnuAsmOperand {
            symbolic_name: i,
            constraints: s,
            variable_name: e,
        }
    }

////
// GNU expression extensions
////

rule gnu_primary_expression() -> Expression =
    statement_expression() /
    offsetof_expression() /
    va_arg_expression() /
    keyword_expression()

rule statement_expression() -> Expression =
    "(" _ s:scoped(<node(<compound_statement()>)>) _ ")" { Expression::Statement(Box::new(s)) }

rule va_arg_expression() -> Expression =
    n:node(<va_arg_expression_inner()>) { Expression::VaArg(Box::new(n)) }

rule va_arg_expression_inner() -> VaArgExpression =
    Ks("__builtin_va_arg") _ "(" _ e:assignment_expression() _ "," _ t:type_name() _ ")" {
        VaArgExpression {
            va_list: e,
            type_name: t,
        }
    }

rule keyword_expression() -> Expression =
    k:node(<$(keyword_expression0())>) {
        let ident = Identifier {
            name: k.node.to_string(),
        };
        Expression::Identifier(Box::new(Node::new(ident, k.span)))
    }

rule keyword_expression0() =
    Ks("__func__") /
    Ks("__FUNCTION__") /
    Ks("__PRETTY_FUNCTION__")

rule offsetof_expression() -> Expression =
    n:node(<offsetof_expression_inner()>) { Expression::OffsetOf(Box::new(n)) }

rule offsetof_expression_inner() -> OffsetOfExpression =
    Ks("__builtin_offsetof") _ "(" _ t:type_name() _ "," _ d:node(<offsetof_designator()>) _ ")" {
        OffsetOfExpression {
            type_name: t,
            designator: d,
        }
    }

rule offsetof_designator() -> OffsetDesignator =
    i:identifier() _ d:list0(<node(<offsetof_member()>)>) {
        OffsetDesignator {
            base: i,
            members: d,
        }
    }

rule offsetof_member() -> OffsetMember =
    "." _ i:identifier(){ OffsetMember::Member(i) } /
    "->" _ i:identifier(){ OffsetMember::IndirectMember(i) } /
    "[" _ e:node(<expression0()>) _ "]" { OffsetMember::Index(e) }

////
// GNU typeof extension
////

rule typeof_specifier() -> TypeSpecifier =
    K(<"typeof" / "__typeof" "__"?>) _ "(" _ e:node(<typeof_specifier0()>) _ ")" { TypeSpecifier::TypeOf(e) }

rule typeof_specifier0() -> TypeOf =
    e:node(<expression0()>) { TypeOf::Expression(e) } /
    t:type_name(){ TypeOf::Type(t) }

////
// ISO/IEC TS 18661 series floating point extensions
////

rule ts18661_float_type_specifier() -> TS18661FloatType =
    ts18661_binary_float() /
    ts18661_decimal_float()

rule ts18661_binary_float() -> TS18661FloatType =
    "_Float" width:ts18661_binary_width() extended:"x"? {
        ts18661_float(true, width, extended.is_some())
    }

rule ts18661_binary_width() -> usize =
    n:$("16" / "32" / "64" / "128") {
        n.parse().unwrap()
    }

rule ts18661_decimal_float() -> TS18661FloatType =
    "_Decimal" width:ts18661_decimal_width() extended:"x"? {
        ts18661_float(false, width, extended.is_some())
    }

rule ts18661_decimal_width() -> usize =
    n:$("32" / "64" / "128") {
        n.parse().unwrap()
    }

rule ts18661_float_suffix() -> TS18661FloatType =
    ("df" / "DF") { ts18661_float(false, 32, false) } /
    ("dd" / "DD") { ts18661_float(false, 64, false) } /
    ("dl" / "DL") { ts18661_float(false, 128, false) } /

    ['f'|'F'] width:ts18661_binary_width()extended:"x"? {
        ts18661_float(true, width, extended.is_some())
    } /
    ['d'|'D'] width:ts18661_decimal_width()extended:"x"? {
        ts18661_float(false, width, extended.is_some())
    }

////
// Clang extensions
////

rule clang<T>(E: rule<T>) -> T = &clang_guard() e:E(){ e }

rule clang_guard() = {? if env.extensions_clang { Ok(()) } else { Err("clang extensions disabled") } }

////
// Windows extensions (-f<no>-ms-extensions in clang)
////

rule ms<T>(E: rule<T>) -> T = &ms_guard() e:E(){ e }

rule ms_guard() = {? if env.extensions_windows { Ok(()) } else { Err("Windows extensions disabled") } }

// MS extension integer types. __int8, __int16, __int32, and __int64 are explicity synonyms of char,
// short, int, and long long, respectively.
// Ref: https://docs.microsoft.com/en-us/cpp/cpp/data-type-ranges
rule ms_integer_types() -> TypeSpecifier =
    Ks("__int8") { TypeSpecifier::Char } /
    Ks("__int16") { TypeSpecifier::Short } /
    Ks("__int32") { TypeSpecifier::Int } /
    Ks("__int64") { TypeSpecifier::Int64 }

rule ms_extension_specifier_list() -> Vec<Node<Extension>> =
    Ks("__declspec") _ "(" _ a:cs0(<node(<declspec()>)>) _ ")" { a } /
    a:list1(<node(<ms_extension_specifier()>)>) { a }

rule declspec() -> Extension =
    n:node(<attribute_name()>) _ p:attribute_parameters()? {
        Extension::Declspec(Attribute {
            name: n,
            arguments: p.unwrap_or_default(),
        })
    }

rule ms_extension_specifier() -> Extension =
    n:node(<$(ms_extension_specifier_keyword())>) {
        Extension::Attribute(Attribute {
            name: Node::new(n.node.to_string(), n.span),
            arguments: Default::default(),
        })
    }

rule ms_extension_specifier_keyword() =
    Ks("__cdecl") /
    Ks("__inline")

});

pub use self::langc::*;

# Aprašomi tokens
from ast import Expression, keyword
from importlib.util import set_loader

import string
from playsound import playsound
from symtable import Symbol

T_INT = 'INT'
T_FLOAT = 'FLOAT'
T_IDENTIFIER = 'IDENTIFIER'
T_KEYWORD = 'KEYWORD'
T_PLUS = 'PLUS'
T_MINUS = 'MINUS'
T_MUL = 'MUL'
T_DIV = 'DIV'
T_POW = 'POW'
T_EQ = 'EQ'
T_EE = 'EE'  # double equals
T_NE = 'NE'
T_LT = 'LT'
T_GT = 'GT'
T_LTE = 'LTE'
T_GTE = 'GTE'
T_OPARENTHESES = 'OPARENTHESES'
T_CPARENTHESES = 'CPARENTHESES'
T_END = 'END'

KEYWORDS = ['value', 'and', 'or', 'not', 'if', 'then', 'elif', 'else', 'for', 'to', 'step', 'while']
NUMBERS = '0123456789'  # naudojama aptikti skaičius, jog galima būtų juos paversti į tokens
LETTERS = string.ascii_letters
LETTERS_NUMBERS = LETTERS + NUMBERS


# Tokens klasė
class Token:
    def __init__(self, type_,
                 value=None):  # value pradinė reikšmė None, kadangi kai kurie tokens (pvz. +) neturi reikšmės
        self.type = type_
        self.value = value

    def matches(self, type_, value):
        return self.type == type_ and self.value == value

    # Representation metodas, atliekamas automatiškai ir skirtas gražesniam atspausdinimui (pvz. rašant 1 + 2, bus gražiai atspausdinta INT:1 (tipas ir reikšmė), PLUS (tik tipas, kadangi nėra reikšmės), INT:2)
    def __repr__(self):
        if self.value:
            return f'{self.type}:{self.value}'
        else:
            return f'{self.type}'


# klaidų klasė, skirta formuoti pranešimus, įvedus neteisingus simbolius
class Error:
    def __init__(self, error_name, info):
        self.error_name = error_name  # klaidos vardas
        self.info = info  # klaidos informacija

    # metodas klaidos atspausdinimui
    def to_string(self):
        error_res = f'{self.error_name}: {self.info}\n'
        return error_res


# klasė skirta gaudyti neleistinus įvestus simbolius (kurie neaprašyti lexer klasėje)
class CharError(Error):
    def __init__(self, info):
        super().__init__('Neleistinas simbolis', info)


# klasė skirta pranešti, kad tikimasi kažkokio simbolio išraiškoje
class ExpectedCharError(Error):
    def __init__(self, info):
        super().__init__('Tikimasi simbolio ', info)


class SyntaxError(Error):
    def __init__(self, info=''):
        super().__init__('Neleistina sintaksė', info)


class RTError(Error):
    def __init__(self, info):  # perduodamas error informacija
        super().__init__('Neleistinas veiksmas', info)


# Lexer klasė, skirta viską suskaidyti į tokens (pvz. 1 + 2 būtų suskaldytą į trys tokens: token(int, 1), token(plus, +) ir token(int, 2)
class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = -1  # sekama simbolio pozicija
        self.current_char = None  # simbolis, kuris dabar tikrinamas
        self.advance()

    # metodas skirtas pereiti prie sekančio simbolio
    def advance(self):
        self.pos += 1
        if self.pos < len(self.text):  # jeigu nepasiektas įvesto teksto galas
            self.current_char = self.text[self.pos]  # esamas simbolis nustatomas į dabar esamo sekantį
        else:  # jeigu pasiektas įvesto teksto galas
            self.current_char = None

    def make_tokens(self):
        tokens = []  # skirtas laikyti tokens
        while self.current_char != None:
            if self.current_char in ' \t':  # įvestame tekste ignoruojami tarpai ar tabuliacijos
                self.advance()
            elif self.current_char in LETTERS:  # jeigu simbolis yra +, į tokens[] pridedamas naujas token T_PLUS
                tokens.append(self.make_identifier())
            elif self.current_char == '+':  # jeigu simbolis yra +, į tokens[] pridedamas naujas token T_PLUS
                playsound('sounds/1.mp3')
                tokens.append(Token(T_PLUS))
                self.advance()
            elif self.current_char == '-':  # jeigu simbolis yra -, į tokens[] pridedamas naujas token T_MINUS
                playsound('sounds/2.mp3')
                tokens.append(Token(T_MINUS))
                self.advance()
            elif self.current_char == '*':  # jeigu simbolis yra *, į tokens[] pridedamas naujas token T_MUL
                playsound('sounds/3.mp3')
                tokens.append(Token(T_MUL))
                self.advance()
            elif self.current_char == '/':  # jeigu simbolis yra /, į tokens[] pridedamas naujas token T_DIV
                playsound('sounds/4.mp3')
                tokens.append(Token(T_DIV))
                self.advance()
            elif self.current_char == '^':  # jeigu simbolis yra ^, į tokens[] pridedamas naujas token T_POW
                tokens.append(Token(T_POW))
                self.advance()
            elif self.current_char == '(':  # jeigu simbolis yra (, į tokens[] pridedamas naujas token T_OPARENTHESES
                tokens.append(Token(T_OPARENTHESES))
                self.advance()
            elif self.current_char == ')':  # jeigu simbolis yra ), į tokens[] pridedamas naujas token T_CPARENTHESES
                tokens.append(Token(T_CPARENTHESES))
                self.advance()
            elif self.current_char in NUMBERS:  # jeigu simbolis yra 0-9, formuojamas skaičius (int arba float)
                tokens.append(self.make_number())
            elif self.current_char == '!':
                tok, error = self.make_not_equals()  # patikrins ar po ! yra = zenklas jei taip, no error
                if error: return [], error
                tokens.append(tok)
            elif self.current_char == '=':  # jeigu simbolis yra =, į tokens[] pridedamas naujas token T_EQ
                tokens.append(self.make_equals())
            elif self.current_char == '<':  # jeigu simbolis yra <, į tokens[] pridedamas naujas token T_LT
                tokens.append(self.make_less_than())
            elif self.current_char == '>':  # jeigu simbolis yra >, į tokens[] pridedamas naujas token T_GT
                tokens.append(self.make_greater_than())

            else:  # jeigu įvedamas neaprašytas simbolis, metama klaida
                illegal_char = self.current_char
                self.advance()
                return [], CharError("\"" + illegal_char + "\"")  # gražinamas tuščias sąrašas ir neleistinas simbolis
        tokens.append(Token(T_END))
        return tokens, None  # None siunčiamas dėl to, nes nėra klaidos

    # metodas skirtas skaičių formavimui (int arba float)
    def make_number(self):
        dot_cnt = 0  # sekamas taškų kiekis, jog būtų galimą žinoti ar tai float ar int tipo skaičius
        number_string = ''  # formuojamas skaičius

        while self.current_char != None and self.current_char in NUMBERS + '.':  # einama pro simbolius, kol jie intervale 0-9 ARBA yra taškas
            if self.current_char == '.':  # jeigu simbolis yra taškas
                if dot_cnt == 1:
                    break  # jeigu yra daugiau negu vienas taškas baigiamas metodas, nes negalima turėti daugiau negu vieno taško
                else:
                    dot_cnt += 1  # sekamas taškų skaičius
                    number_string += '.'  # prie formuojamo skaičiaus pridedamas taškas
            else:  # jeigu simbolis yra intervale 0-9
                number_string += self.current_char  # formuojamas skaičius
            self.advance()

        if dot_cnt == 0:  # jeigu suformuotame skaičiuje nėra taškų, tai bus int
            return Token(T_INT, int(number_string))
        else:  # jeigu suformuotame skaičiuje yra taškas, tai bus float
            return Token(T_FLOAT, float(number_string))

    def make_identifier(self):
        id_str = ''

        # pildomas string for keyword token arba identifier token
        while self.current_char != None and self.current_char in LETTERS_NUMBERS + '_':
            id_str += self.current_char
            self.advance()

        # nusprendžiamas tipas
        tok_type = T_KEYWORD if id_str in KEYWORDS else T_IDENTIFIER
        return Token(tok_type, id_str)

    def make_not_equals(self):
        self.advance()  # pirmasis simbolis jau zinoma, kad '!'

        if self.current_char == '=':
            self.advance()
            return Token(T_NE), None  # None, nes nera error

        self.advance()
        return None, ExpectedCharError("Trūksta '=' po '!' ženklo")  # error, nes nera !=

    def make_equals(self):
        tok_type = T_EQ
        self.advance()  # jau zinoma, kad einamasis simbolis yra '=' , tai pereinama toliau
        if self.current_char == '=':  # jei antrasis simbolis irgi =, tai sukuriamas double equals, kitu atvejus single
            self.advance()
            tok_type = T_EE

        return Token(tok_type)

    def make_less_than(self):
        tok_type = T_LT
        self.advance()
        if self.current_char == '=':
            self.advance()
            tok_type = T_LTE

        return Token(tok_type)

    def make_greater_than(self):
        tok_type = T_GT
        self.advance()  # jau zinoma, kad einamasis simbolis yra '>' , tai pereinama toliau
        if self.current_char == '=':  # jei antrasis simbolis  =, tai sukuriamas greater or equal, kitu atveju greater
            self.advance()
            tok_type = T_GTE

        return Token(tok_type)


# klasė parser'iui priimti skaičius
class NumberNode:
    def __init__(self, token):
        self.token = token

    # metodas gražiam atspausdinimui
    def __repr__(self):
        return f'{self.token}'


class ValueAccessNode:
    def __init__(self, value_name_tok):
        self.value_name_tok = value_name_tok


class ValueAssignNode:
    def __init__(self, value_name_tok, value_node):
        self.value_name_tok = value_name_tok
        self.value_node = value_node


# klase parser'iui paprastoms operacijoms (sudėtis, atimtis, dalyba, daugyba)
class SimpleOperatorNode:
    def __init__(self, left_node, operator_token,
                 right_node):  # pvz. 1 + 2 - 1 (leftnode), + (operator_token), 2 (right_node)
        self.left_node = left_node
        self.operator_token = operator_token
        self.right_node = right_node

    # metodas gražiam atspausdinimui
    def __repr__(self):
        return f'({self.left_node}, {self.operator_token}, {self.right_node})'


# klasė parser'iui dirbtu su tokiomis operacijomis, kaip -5
class UnaryOperatorNode:
    def __init__(self, operator_token, node):
        self.operator_token = operator_token
        self.node = node

    # metodas gražiam atspausdinimui
    def __repr__(self):
        return f'({self.operator_token}, {self.node})'


class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case


class ForNode:
    def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body_node):
        self.var_name_tok = var_name_tok
        self.start_value_node = start_value_node
        self.end_value_node = end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_node


class WhileNode:
    def __init__(self, condition_node, body_node):
        self.condition_node = condition_node
        self.body_node = body_node


# klasė skirta patikrinti ar parser'io rezultatas neturi klaidų
class ParseResult:
    def __init__(self):
        self.error = None  # jeigu yra klaida
        self.node = None
        self.advance_count = 0  # skaiciuoja kiek kartu buvo advanced funkcijoje

    # skirtas tik for advancements
    def register_advancement(self):
        self.advance_count += 1

    def register(self, result):  # priima kita parser'io rezultatą
        self.advance_count += result.advance_count
        if result.error: self.error = result.error  # jeigu result turi klaidos pranešimą, jus nustatomas
        return result.node

    def success(self, node):
        self.node = node
        return self

    def fail(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self


# parser'io klasė
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1  # sekti esamą token
        self.advance()  # pereiti prie kito token

    def advance(self, ):
        self.token_index += 1
        if self.token_index < len(self.tokens):  # jeigu nepasiektas galas
            self.current_token = self.tokens[self.token_index]
        return self.current_token

    def parse(self):
        result = self.expression()
        if not result.error and self.current_token.type != T_END:  # jeigu nėra klaidos ir dar nepasiektas galas
            return result.fail(
                SyntaxError("Reikia '+', '-', '*' or '/'"))  # metama klaida, jog trūksta operacijos ženklo
        return result

    def atom(self):
        result = ParseResult()
        token = self.current_token

        if token.type in (T_INT, T_FLOAT):  # toliau jeigu token yra int arba float
            result.register_advancement()
            self.advance()
            return result.success(NumberNode(token))

        elif token.type == T_IDENTIFIER:
            result.register_advancement()
            self.advance()
            return result.success(ValueAccessNode(token))

        elif token.type == T_OPARENTHESES:  # toliau jeigu token yra simbolis '('
            result.register_advancement()
            self.advance()
            expression = result.register(self.expression())  # gaunamas naujas išsireiškimas
            if result.error: return result
            if self.current_token.type == T_CPARENTHESES:  # jeigu token yra simbolis ')' (uždaromi skliaustai)
                result.register_advancement()
                self.advance()
                return result.success(expression)
            else:  # jeigu nerandami tinkami skliaustai
                return result.fail(SyntaxError("Trūksta simbolio ')'"))

        elif token.matches(T_KEYWORD, 'if'):
            if_expr = result.register(self.if_expr())
            if result.error: return result
            return result.success(if_expr)

        elif token.matches(T_KEYWORD, 'for'):
            for_expr = result.register(self.for_expr())
            if result.error: return result
            return result.success(for_expr)

        elif token.matches(T_KEYWORD, 'while'):
            while_expr = result.register(self.while_expr())
            if result.error: return result
            return result.success(while_expr)

        return result.fail(SyntaxError("Tikimasi int, float, identifikatoriaus, '+', '-', arba '('"))

    def power(self):
        return self.simple_operator(self.atom, (T_POW,), self.factor)

    # metodas dirbti su skaičių išsireiškimais
    def factor(self):
        result = ParseResult()
        token = self.current_token

        if token.type in (T_PLUS, T_MINUS):  # jeigu token yra + arba -
            result.register_advancement()
            self.advance()
            factor = result.register(self.factor())  # gaunamas naujas išsireiškimas
            if result.error:  # jeigu yra klaida
                return result
            return result.success(UnaryOperatorNode(token, factor))
        return self.power()

    # metodas dirbti su išsireiškimais, turinčiais daugybą ar dalybą
    def term(self):
        return self.simple_operator(self.factor, (T_MUL, T_DIV))

    def comp_expr(self):
        res = ParseResult()

        # jei token yra ne not, tai reiskia, kad tai yra aritmetine israiska
        if self.current_token.matches(T_KEYWORD, 'not'):
            op_tok = self.current_token
            res.register_advancement()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error: return res
            return res.success(UnaryOperatorNode(op_tok, node))

        node = res.register(
            self.simple_operator(self.arith_expr, (T_EE, T_NE, T_LT, T_GT, T_LTE, T_GTE)))  # galimi zenklai

        if res.error:
            return res.fail(SyntaxError("Tikimasi int, float, identifikatoriaus, '+', '-', '(', 'not'"))

        return res.success(node)

    def arith_expr(self):
        return self.simple_operator(self.term, (T_PLUS, T_MINUS))

    # metodas dirbti su išsireiškimais, turinčiais sudėtį ar atimtį
    def expression(self):
        res = ParseResult()

        # pereinama per tokens jei prasideda zodziu "value"
        # pirma value paskui Eq paskui išraiška
        if self.current_token.matches(T_KEYWORD, 'value'):
            res.register_advancement()
            self.advance()

            if self.current_token.type != T_IDENTIFIER:
                return res.fail(SyntaxError("Trūksta identifikatoriaus"))

            value_name = self.current_token
            res.register_advancement()
            self.advance()

            if self.current_token.type != T_EQ:
                return res.fail(SyntaxError("Trūksta '=' ženklo"))

            res.register_advancement()
            self.advance()
            expression = res.register(self.expression())
            if res.error: return res
            return res.success(ValueAssignNode(value_name, expression))

        node = res.register(self.simple_operator(self.comp_expr, ((T_KEYWORD, "and"), (T_KEYWORD, "or"))))
        if res.error:
            return res.fail(SyntaxError("Tikimasi int, float, identifikatoriaus, 'value' '+', '-', arba '('"))
        return res.success(node)

    # metodas dirbti su išsireiškimais, kaip 1 + 2 ar (1 + 2) * 3) pagal poreikį
    def simple_operator(self, func_a, operators,
                        func_b=None):  # poreikis nusprendžiamas pagal gautą term arba expression (func) ir ar +/-, ar dalyba/daugyba (operators)
        if func_b == None:
            func_b = func_a
        result = ParseResult()
        left = result.register(func_a())  # gaunamas kairysis skaičius
        if result.error:  # jeigu yra klaida
            return result

        while self.current_token.type in operators or (self.current_token.type, self.current_token.value) in operators:
            operator_token = self.current_token
            result.register_advancement()
            self.advance()
            right = result.register(func_b())  # gaunamas dešinysis skaičius
            if result.error:  # jeigu yra klaida
                return result
            left = SimpleOperatorNode(left, operator_token, right)

        return result.success(left)

    def if_expr(self):
        res = ParseResult()
        cases = []
        else_case = None

        if not self.current_token.matches(T_KEYWORD, 'if'):
            failMes = f"Expected 'IF'"
            return res.fail(SyntaxError(failMes))

        res.register_advancement()
        self.advance()
        condition = res.register(self.expression())

        if res.error: return res
        if not self.current_token.matches(T_KEYWORD, 'then'):
            failMes = f"Expected 'then'"
            return res.fail(SyntaxError(
                failMes
            ))
        res.register_advancement()
        self.advance()

        expr = res.register(self.expression())
        if res.error: return res
        cases.append((condition, expr))

        while self.current_token.matches(T_KEYWORD, 'elif'):
            res.register_advancement()
            self.advance()

            condition = res.register(self.expression())
            if res.error: return res

            if not self.current_token.matches(T_KEYWORD, 'then'):
                failMes = f"Expected 'then/elif'"
                return res.fail(failMes)

            res.register_advancement()
            self.advance()

            expr = res.register(self.expression())
            if res.error: return res
            cases.append((condition, expr))

        if self.current_token.matches(T_KEYWORD, 'else'):
            res.register_advancement()
            self.advance()

            else_case = res.register(self.expression())
            if res.error: return res

        return res.success(IfNode(cases, else_case))

    def for_expr(self):
        res = ParseResult()

        if not self.current_token.matches(T_KEYWORD, 'for'):
            return res.fail(SyntaxError(
                f"Expected 'FOR'"
            ))
        res.register_advancement()
        self.advance()
        if self.current_token.type != T_IDENTIFIER:
            return res.fail(SyntaxError(
                f"Expected identifier"
            ))
        var_name = self.current_token
        res.register_advancement()
        self.advance()

        if self.current_token.type != T_EQ:
            return res.fail(SyntaxError(
                f"Expected '='"
            ))
        res.register_advancement()
        self.advance()

        start_value = res.register(self.expression())
        if res.error:
            return res

        if not self.current_token.matches(T_KEYWORD, 'to'):
            return res.fail(SyntaxError(
                f"Expected 'TO'"
            ))
        res.register_advancement()
        self.advance()
        end_value = res.register(self.expression())
        if res.error:
            return res
        if self.current_token.matches(T_KEYWORD, 'step'):
            res.register_advancement()
            self.advance()
            step_value = res.register(self.expression())
            if res.error: return res
        else:
            step_value = None
        if not self.current_token.matches(T_KEYWORD, 'then'):
            return res.fail(SyntaxError(
                f"Expected'THEN'"
            ))

        res.register_advancement()
        self.advance()

        body = res.register(self.expression())
        if res.error: return res

        return res.success(ForNode(var_name, start_value, end_value, step_value, body))

    def while_expr(self):
        res = ParseResult()

        if not self.current_token.matches(T_KEYWORD, 'while'):
            return res.fail(SyntaxError(
                f"Expected 'WHILE'"
            ))
        res.register_advancement()
        self.advance()
        condition = res.register(self.expression())
        if res.error: return res
        if not self.current_token.matches(T_KEYWORD, 'then'):
            return res.fail(SyntaxError(
                f"Expected'THEN'"))
        res.register_advancement()
        self.advance()

        body = res.register(self.expression())
        if res.error: return res

        return res.success(WhileNode(condition, body))


# klasė run time errorams
class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error:
            self.error = res.error
        return res.value

    def success(self, value):
        self.value = value
        return self

    def failure(self, error):
        self.error = error
        return self


# Skaičių klasė ju laikymui ir operavimui su jais
class Number:
    def __init__(self, value):
        self.value = value

    def added_to(self, other):  # gražina sudėties rezultata
        if isinstance(other, Number):  # jei reiksmė yra kitas skaičius
            return Number(self.value + other.value), None  # None tai kad nera error atliekant operacija

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value), None

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value), None

    # gali but div by 0 tai reikia patikrinti kad nebutu error
    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:  # tikrinimas ar ne nulis
                return None, RTError('Division by zero')  # value = None, Error = division by zero
            return Number(self.value / other.value), None

    # kelimas ( ^ )
    def powered_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value), None

    # comparison metodai
    # 0 = false, 1 = true yra palyginama reiksme ir grazinama boolean'o int() rezultatas ir None error
    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)), None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)), None

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)), None

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)), None

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)), None

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)), None

    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)), None

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)), None

    def notted(self):
        return Number(1 if self.value == 0 else 0), None

    def is_true(self):
        return self.value != 0

    def __repr__(self):
        return str(self.value)


class Context:
    def __init__(self, display_name, parent=None):
        self.display_name = display_name
        self.parent = parent
        self.symbol_table = None


# simboliu class

class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def set(self, name, value):
        self.symbols[name] = value

    def remove(self, name):
        del self.symbols[name]


# interpretatoriaus class
class Interpreter:
    def visit(self, node, context):  # pereina per nodes pagal node tipą
        method_name = f'visit_{type(node).__name__}'  # indikatorius parodantis node tipą
        method = getattr(self, method_name, self.no_visit_method)  # gaunama kuris metodas turi but iškviestas
        return method(node, context)

    def no_visit_method(self, node, context):  # default iškviečiamas metodas
        raise Exception(f'No visit_{type(node).__name__} method defined')

    # metodai pagal tipus
    def visit_NumberNode(self, node, context):
        return RTResult().success(Number(node.token.value))  # sukuriamas skaicius ir jis visados successful

    def visit_ValueAccessNode(self, node, context):
        res = RTResult()
        value_name = node.value_name_tok.value
        value = context.symbol_table.get(value_name)

        # jei nebuvo aprasytas kintamasis
        if not value:
            return res.failure(RTError(f"'{value_name} nerastas"))

        return res.success(value)

    def visit_ValueAssignNode(self, node, context):
        res = RTResult()
        value_name = node.value_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.error: return res

        context.symbol_table.set(value_name, value)
        return res.success(value)

    def visit_SimpleOperatorNode(self, node, context):
        res = RTResult()  # runTime result class instance
        left = res.register(self.visit(node.left_node, context))  # res.register() tikrina ar nera error
        if res.error:  # tikrinama ar nera jokiu errors
            return res

        right = res.register(self.visit(node.right_node, context))
        if res.error:  # tikrinama ar nera jokiu errors
            return res

        # tikrinama kuris operatorius
        if node.operator_token.type == T_PLUS:
            result, error = left.added_to(right)  # nustato ir rezultata ir error
        elif node.operator_token.type == T_MINUS:
            result, error = left.subbed_by(right)
        elif node.operator_token.type == T_MUL:
            result, error = left.multed_by(right)
        elif node.operator_token.type == T_DIV:
            result, error = left.dived_by(right)
        elif node.operator_token.type == T_POW:
            result, error = left.powered_by(right)
        elif node.operator_token.type == T_EE:
            result, error = left.get_comparison_eq(right)
        elif node.operator_token.type == T_NE:
            result, error = left.get_comparison_ne(right)
        elif node.operator_token.type == T_LT:
            result, error = left.get_comparison_lt(right)
        elif node.operator_token.type == T_GT:
            result, error = left.get_comparison_gt(right)
        elif node.operator_token.type == T_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.operator_token.type == T_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.operator_token.matches(T_KEYWORD, 'and'):
            result, error = left.anded_by(right)
        elif node.operator_token.matches(T_KEYWORD, 'or'):
            result, error = left.ored_by(right)

        # jei randa error nusiuncia i runtime error class failure, kitu atveju success
        if error:
            return res.failure(error)
        else:
            return res.success(result)

    def visit_UnaryOperatorNode(self, node, context):
        res = RTResult()  # runTime result class instance

        number = res.register(self.visit(node.node, context))  # tikrina ar nera error su unary operatorium
        if res.error:
            return res

        # padauginamas skaičius iš -1
        if node.operator_token.type == T_MINUS:
            number, error = number.multed_by(Number(-1))  # grazina ir number ir error
        elif node.operator_token.matches(T_KEYWORD, 'not'):  # 1 => 0   0 => 1
            nubmer, error = number.notted()

        if error:
            return res.failure(error)  # siuncia i failure jei rastas error
        else:
            return res.success(number)

    def visit_IfNode(self, node, context):
        res = RTResult()

        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.error: return res

            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.error: return res
                return res.success(expr_value)

        if node.else_case:
            else_value = res.register(self.visit(node.else_case, context))
            if res.error: return res
            return res.success(else_value)

        return res.success(None)

    def visit_ForNode(self, node, context):
        res = RTResult()

        start_value = res.register(self.visit(node.start_value_node, context))
        if res.error: return res
        end_value = res.register(self.visit(node.end_value_node, context))
        if res.error: return res
        if node.step_value_node:
            step_value = res.register(self.visit(node.step_value_node, context))
            if res.error: return res
        else:
            step_value = Number(1)

        i = start_value.value

        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value
        while condition():
            context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value

            res.register(self.visit(node.body_node, context))
            if res.error: return res

        return res.success(None)

    def visit_WhileNode(self, node, context):
        res = RTResult()

        while True:
            condition = res.register(self.visit(node.condition_node, context))
            if res.error: return res

            if not condition.is_true(): break

            res.register(self.visit(node.body_node, context))
            if res.error: return res

        return res.success(None)


global_symbol_table = SymbolTable()
global_symbol_table.set("null", Number(0))
global_symbol_table.set("true", Number(1))
global_symbol_table.set("false", Number(0))


# lexer'io paleidimui
def run(text):
    lexer = Lexer(text)  # sukūriamas lexer'is su įvestu tekstu
    tokens, error = lexer.make_tokens()  # gaunami tokens ir klaida, jeigu ji yra (None jeigu jos nėra)
    if error:
        return None, error

    parser = Parser(tokens)  # sukūriamas parser'is su token'ais

    # generuojamas AST (abstrakčios sintaksės medis)
    astree = parser.parse()

    if astree.error:
        return None, astree.error

    # sukuriamas interpretatorius
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(astree.node, context)  # perduodamas astree node interpretatoriui pereit

    return result.value, result.error

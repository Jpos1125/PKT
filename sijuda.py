# Aprašomi tokens
T_INT = 'INT'
T_FLOAT = 'FLOAT'
T_PLUS = 'PLUS'
T_MINUS = 'MINUS'
T_MUL = 'MUL'
T_DIV = 'DIV'
T_OPARENTHESES = 'OPARENTHESES'
T_CPARENTHESES = 'CPARENTHESES'
T_END = 'END'

NUMBERS = '0123456789' # naudojama aptikti skaičius, jog galima būtų juos paversti į tokens

# Tokens klasė
class Token:
    def __init__(self, type_, value=None): # value pradinė reikšmė None, kadangi kai kurie tokens (pvz. +) neturi reikšmės
        self.type = type_
        self.value = value

    # Representation metodas, atliekamas automatiškai ir skirtas gražesniam atspausdinimui (pvz. rašant 1 + 2, bus gražiai atspausdinta INT:1 (tipas ir reikšmė), PLUS (tik tipas, kadangi nėra reikšmės), INT:2)
    def __repr__(self):
        if self.value:
            return f'{self.type}:{self.value}'
        else:
            return f'{self.type}'

# klaidų klasė, skirta formuoti pranešimus, įvedus neteisingus simbolius
class Error:
    def __init__(self, error_name, info):
        self.error_name = error_name # klaidos vardas
        self.info = info # klaidos informacija

    # metodas klaidos atspausdinimui
    def to_string(self):
        error_res = f'{self.error_name}: {self.info}\n'
        return error_res

# klasė skirta gaudyti neleistinus įvestus simbolius (kurie neaprašyti lexer klasėje)
class CharError(Error):
    def __init__(self, info):
        super().__init__('Neleistinas simbolis', info)

class SyntaxError(Error):
    def __init__(self, info=''):
        super().__init__('Neleistina sintaksė', info)

# Lexer klasė, skirta viską suskaidyti į tokens (pvz. 1 + 2 būtų suskaldytą į trys tokens: token(int, 1), token(plus, +) ir token(int, 2)
class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = -1  # sekama simbolio pozicija
        self.current_char = None # simbolis, kuris dabar tikrinamas
        self.advance()

    # metodas skirtas pereiti prie sekančio simbolio
    def advance(self):
        self.pos += 1
        if self.pos < len(self.text): # jeigu nepasiektas įvesto teksto galas
            self.current_char = self.text[self.pos] # esamas simbolis nustatomas į dabar esamo sekantį
        else: # jeigu pasiektas įvesto teksto galas
            self.current_char = None

    def make_tokens(self):
        tokens = [] # skirtas laikyti tokens

        while self.current_char != None:
            if self.current_char in ' \t': # įvestame tekste ignoruojami tarpai ar tabuliacijos
                self.advance()
            elif self.current_char == '+': # jeigu simbolis yra +, į tokens[] pridedamas naujas token T_PLUS
                tokens.append(Token(T_PLUS))
                self.advance()
            elif self.current_char == '-': # jeigu simbolis yra -, į tokens[] pridedamas naujas token T_MINUS
                tokens.append(Token(T_MINUS))
                self.advance()
            elif self.current_char == '*': # jeigu simbolis yra *, į tokens[] pridedamas naujas token T_MUL
                tokens.append(Token(T_MUL))
                self.advance()
            elif self.current_char == '/': # jeigu simbolis yra /, į tokens[] pridedamas naujas token T_DIV
                tokens.append(Token(T_DIV))
                self.advance()
            elif self.current_char == '(': # jeigu simbolis yra (, į tokens[] pridedamas naujas token T_OPARENTHESES
                tokens.append(Token(T_OPARENTHESES))
                self.advance()
            elif self.current_char == ')': # jeigu simbolis yra ), į tokens[] pridedamas naujas token T_CPARENTHESES
                tokens.append(Token(T_CPARENTHESES))
                self.advance()
            elif self.current_char in NUMBERS: # jeigu simbolis yra 0-9, formuojamas skaičius (int arba float)
                tokens.append(self.make_number())
            else: # jeigu įvedamas neaprašytas simbolis, metama klaida
                illegal_char = self.current_char
                self.advance()
                return [], CharError("\"" + illegal_char + "\"")  # gražinamas tuščias sąrašas ir neleistinas simbolis
        tokens.append(Token(T_END))
        return tokens, None # None siunčiamas dėl to, nes nėra klaidos

    # metodas skirtas skaičių formavimui (int arba float)
    def make_number(self):
        dot_cnt = 0  # sekamas taškų kiekis, jog būtų galimą žinoti ar tai float ar int tipo skaičius
        number_string = '' # formuojamas skaičius

        while self.current_char != None and self.current_char in NUMBERS + '.': # einama pro simbolius, kol jie intervale 0-9 ARBA yra taškas
            if self.current_char == '.': # jeigu simbolis yra taškas
                if dot_cnt == 1:
                    break # jeigu yra daugiau negu vienas taškas baigiamas metodas, nes negalima turėti daugiau negu vieno taško
                else:
                    dot_cnt += 1 # sekamas taškų skaičius
                    number_string += '.' # prie formuojamo skaičiaus pridedamas taškas
            else: # jeigu simbolis yra intervale 0-9
                number_string += self.current_char # formuojamas skaičius
            self.advance()

        if dot_cnt == 0: # jeigu suformuotame skaičiuje nėra taškų, tai bus int
            return Token(T_INT, int(number_string))
        else: # jeigu suformuotame skaičiuje yra taškas, tai bus float
            return Token(T_FLOAT, float(number_string))

# klasė parser'iui priimti skaičius
class NumberNode:
	def __init__(self, token):
		self.token = token

    # metodas gražiam atspausdinimui
	def __repr__(self):
		return f'{self.token}'

# klase parser'iui paprastoms operacijoms (sudėtis, atimtis, dalyba, daugyba)
class SimpleOperatorNode:
	def __init__(self, left_node, operator_token, right_node): # pvz. 1 + 2 - 1 (leftnode), + (operator_token), 2 (right_node)
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

# klasė skirta patikrinti ar parser'io rezultatas neturi klaidų
class ParseResult:
	def __init__(self):
		self.error = None # jeigu yra klaida
		self.node = None

	def register(self, result): # priima kita parser'io rezultatą
		if isinstance(result, ParseResult): # jeigu result yra parser'io rezultatas
			if result.error: self.error = result.error # jeigu result turi klaidos pranešimą, jus nustatomas
			return result.node

		return result

	def success(self, node):
		self.node = node
		return self

	def fail(self, error):
		self.error = error
		return self

# parser'io klasė
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1 # sekti esamą token
        self.advance() # pereiti prie kito token

    def advance(self, ):
        self.token_index += 1
        if self.token_index < len(self.tokens): # jeigu nepasiektas galas
            self.current_token = self.tokens[self.token_index]
        return self.current_token

    def parse(self):
        result = self.expression()
        if not result.error and self.current_token.type != T_END: # jeigu nėra klaidos ir dar nepasiektas galas
            return result.fail(SyntaxError("Reikia '+', '-', '*' or '/'")) # metama klaida, jog trūksta operacijos ženklo
        return result

    # metodas dirbti su skaičių išsireiškimais
    def factor(self):
        result = ParseResult()
        token = self.current_token

        if token.type in (T_PLUS, T_MINUS): # jeigu token yra + arba -
            result.register(self.advance())
            factor = result.register(self.factor()) # gaunamas naujas išsireiškimas
            if result.error: # jeigu yra klaida
                return result
            return result.success(UnaryOperatorNode(token, factor))

        elif token.type in (T_INT, T_FLOAT): # toliau jeigu token yra int arba float
            result.register(self.advance())
            return result.success(NumberNode(token))

        elif token.type == T_OPARENTHESES: # toliau jeigu token yra simbolis '('
            result.register(self.advance())
            expression = result.register(self.expression()) # gaunamas naujas išsireiškimas
            if result.error: return result
            if self.current_token.type == T_CPARENTHESES: # jeigu token yra simbolis ')' (uždaromi skliaustai)
                result.register(self.advance())
                return result.success(expression)
            else: # jeigu nerandami tinkami skliaustai
                return result.fail(SyntaxError("Trūksta simbolio ')'"))

        return result.fail(SyntaxError("Reikia int arba float"))

    # metodas dirbti su išsireiškimais, turinčiais daugybą ar dalybą
    def term(self):
        return self.simple_operator(self.factor, (T_MUL, T_DIV))

    # metodas dirbti su išsireiškimais, turinčiais sudėtį ar atimtį
    def expression(self):
        return self.simple_operator(self.term, (T_PLUS, T_MINUS))

    # metodas dirbti su išsireiškimais, kaip 1 + 2 ar (1 + 2) * 3) pagal poreikį
    def simple_operator(self, func, operators): # poreikis nusprendžiamas pagal gautą term arba expression (func) ir ar +/-, ar dalyba/daugyba (operators)
        result = ParseResult()
        left = result.register(func()) # gaunamas kairysis skaičius
        if result.error: # jeigu yra klaida
            return result

        while self.current_token.type in operators:
            operator_token = self.current_token
            result.register(self.advance())
            right = result.register(func()) # gaunamas dešinysis skaičius
            if result.error: # jeigu yra klaida
                return result
            left = SimpleOperatorNode(left, operator_token, right)

        return result.success(left)

# lexer'io paleidimui
def run(text):
    lexer = Lexer(text)  # sukūriamas lexer'is su įvestu tekstu
    tokens, error = lexer.make_tokens() # gaunami tokens ir klaida, jeigu ji yra (None jeigu jos nėra)
    if error:
        return None, error

    parser = Parser(tokens)  # sukūriamas parser'is su token'ais

    # generuojamas AST (abstrakčios sintaksės medis)
    astree = parser.parse()
    return astree.node, astree.error
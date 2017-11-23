import re

"""
<s-expr> ::= <num>
         | (+ <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (if0 <s-expr> <s-expr> <s-expr>)
         | (with (<sym> <s-expr>) <s-expr>)
         | (fun (<sym>) <s-expr>)
         | (set <sym> <s-expr>)
         | (seqn <s-expr> <s-expr>)
         | (<s-expr> <s-expr>)
         
Analogo para expr, que agrega app en la ultima
"""

# Exceptions


# Utilizada cuando no se encuentra el loc en el store
class LocNotFoundException(Exception):
    """
    Setea el mensaje de excepcion
    Constructor; __init__: Int/String
    """
    def __init__(self, msg):
        self.msg = msg

    # Para imprimir el mensaje de excepcion
    # __str__ :: () -> String
    def __str__(self):
        return "Can't find loc: '" + str(self.msg) + "'"


# Utilizado para cuando no se encuentra el simbolo en el ambiente
class NotFoundInEnvException(Exception):
    """
    Setea el mensaje de excepcion
    Constructor; __init__ : String
    """
    def __init__(self, idx):
        self.idx = idx

    # Para imprimir el mensaje de excepcion
    # __str__ :: () -> String
    def __str__(self):
        return "error: identificador libre!! " + self.idx


# Utilizado cuando no se puede parsear la expresion (no esta dentro del lenguaje
class ParsingException(Exception):
    """
    Setea el mensaje de excepcion
    Constructor; __init__ : String
    """
    def __init__(self, s):
        self.s = s

    # Para imprimir el mensaje de excepcion
    # __str__ :: () -> String
    def __str__(self):
        return "error: expresion invalida " + self.s + ""


# Environment
"""
Environment abstract data type

EmptyEnv:: Env
extendEnv:: Sym Val Env -> Env
lookupEnv:: Sym
Env -> Val

representation
BNF:
< env >::= (EmptyEnv)
        | (aEnv <id> <val> <env>)
"""


# Ambiente vacio para comenzar desde ahi la creacion de ambientes
# Tambien permite encapsular el error de no encontrar un simbolo
class EmptyEnv:
    # Constructor; __init__
    def __init__(self):
        pass

    # lookupEnv : Symbol -> Exception
    @staticmethod
    def lookup_env(idx):
        raise NotFoundInEnvException(idx)


# Clase ambiente principal
class Env:
    # Constructor; __init__ : Symbol Value Env
    # Para extender el ambiente
    def __init__(self, idx, value, next_env):
        self.idx = idx
        self.value = value
        self.nextEnv = next_env

    # Para hacer lookup desde el ambiente actual
    # lookup_env : Symbol -> Env
    def lookup_env(self, idx):
        if self.idx == idx:
            return self.value
        return self.nextEnv.lookup_env(idx)


# extend_env :: Sym Value Env -> Env
def extend_env(idx, value, env):
    return Env(idx, value, env)


# lookup_env :: Sym Env -> Value
def lookup_env(idx, env):
    return env.lookup_env(idx)


# Store
"""
#|-----------------------------
Store abstract data type
 
EmptySto  :: Sto
extendSto :: Loc Val Sto -> Sto
lookupSto :: Loc Sto -> Val
 
representation BNF:
<sto> ::= (EmptySto)
        | (aSto <loc> <val> <sto>)
<loc> ::= number
|#
"""


class EmptySto:
    # Constructor; __init__
    def __init__(self):
        pass

    # lookup_sto : Int -> Exception
    @staticmethod
    def lookup_sto(loc):
        raise LocNotFoundException(loc)

    # next_location : () -> 0 (Int)
    @staticmethod
    def next_location():
        return 0


class Sto:
    # Constructor; __init__ : Int Value Store
    def __init__(self, loc, val, sto):
        self.loc = loc
        self.val = val
        self.nextSto = sto
        pass

    # lookup_sto : Int -> Value
    def lookup_sto(self, loc):
        if loc == self.loc:
            return self.val

        return self.nextSto.lookup_sto(loc)

    # next_location : () -> Int
    def next_location(self):
        return 1 + self.nextSto.next_location()


# lookup_sto :: Int Store -> Int
def lookup_sto(loc, sto):
    return sto.lookup_sto(loc)


# extend_sto Int Int Store -> Store
def extend_sto(loc, val, sto):
    return Sto(loc, val, sto)


# next_location :: Store -> Int
def next_location(sto):
    return sto.next_location()


# ValSto (Value-Store) data structure
# <valsto> = (v*s val sto)
class ValSto:
    # Constructor ; __init__ : Value Store
    def __init__(self, val, sto):
        self.val = val
        self.sto = sto

    # get_val : () -> Value
    def get_val(self):
        return self.val

    # get_sto : () -> Store
    def get_sto(self):
        return self.sto


"""
Value data structures
<Value> ::= <NumV>
        | <ClosureV>
        
Closure for functions:        
<ClosureV> ::= (id body env)
"""


# NumV
# Corresponde al tipo Value, se utiliza como wrapper de el numero explicito
class NumV:
    # Constructor; __init__ : Int
    def __init__(self, num):
        self.num = num

    # get_num : () -> Int
    def get_num(self):
        return self.num

    # Operator overloading of +
    # __add__ : NumV -> NumV
    def __add__(self, other):
        return NumV(self.num + other.num)

    # Operator overloading of -
    # __sub__ : NumV -> NumV
    def __sub__(self, other):
        return NumV(self.num - other.num)

    # __str_ : () -> String
    def __str__(self):
        return str(self.num)


# ClosureV
# Corresponde al tipo Value, se utiliza como Wrapper para funciones y mantener el scope estatico
class ClosureV:
    # Constructor; __init__ : Symbol Expr Env
    def __init__(self, idx, body, env):
        self.idx = idx
        self.body = body
        self.env = env

    # __str__ :  () -> "Function"
    def __str__(self):
        return "Function"


"""
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id s)
  (app fun-expr arg-expr)
  (fun id body)
  (set id val-expr)
  (seqn expr1 expr2))
"""


# Num
# Corresponde a un tipo Expr
class Num:
    # Constructor; __init__ : Int
    def __init__(self, num):
        self.num = num

    # eval : Env Store -> ValSto
    def eval(self, env, sto):
        return ValSto(NumV(self.num), sto)


# BinaryExpr
# Es una superclase de operadores binarios del lenguaje
class BinaryExpr:
    # Constructor; __init__ : Expr Expr
    def __init__(self,  expr1, expr2):
        self.expr1 = expr1
        self.expr2 = expr2


# Corresponde a un tipo Expr. Se utiliza para almacenar expresiones de suma y eventualmente sumar.
class SumExpr(BinaryExpr):
    # eval : Env Sto -> ValSto
    def eval(self, env, sto):
        vs1 = self.expr1.eval(env, sto)
        vs2 = self.expr2.eval(env, vs1.get_sto())
        return ValSto(vs1.get_val() + vs2.get_val(), vs2.get_sto())


# Corresponde a un tipo Expr. Se utiliza para almacenar expresiones de resta y eventualmente restar.
class DiffExpr(BinaryExpr):
    # eval : Env Sto -> ValSto
    def eval(self, env, sto):
        vs1 = self.expr1.eval(env, sto)
        vs2 = self.expr2.eval(env, vs1.get_sto())
        return ValSto(vs1.get_val() - vs2.get_val(), vs2.get_sto())


# Corresponde a un tipo Expr. Se utiliza para almacenar expresiones condicionales y eventualmente procesarlas
class If0Expr:
    # Constructor; __init__ : Expr Expr Expr
    def __init__(self, expr1, expr2, expr3):
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3

    # eval : Env Sto -> ValSto
    def eval(self, env, sto):
        valsto = self.expr1.eval(env, sto)
        if valsto.get_val().get_num() == 0:
            return self.expr2.eval(env, sto)
        else:
            return self.expr3.eval(env, sto)


# Corresponde a un tipo Expr. Se utiliza como wrapper para simbolos y eventualmente hacer lookup
class IdExpr:
    # Constructor; __init__ : Symbol
    def __init__(self, idx):
        self.idx = idx

    # eval : Env Sto -> ValSto
    def eval(self, env, sto):
        loc = lookup_env(self.idx, env)
        return ValSto(lookup_sto(loc, sto), sto)

    # getString : () -> Symbol
    def get_string(self):
        return self.idx


# Corresponde a un tipo Expr. Se utiliza como wrapper para una funcion y
#  recordar el ambiente en tiempo de declaracion
class FunExpr:
    # Constructor; __init__ : Symbol Expr
    def __init__(self, idx, body):
        self.idx = idx
        self.body = body

    # eval : Env Sto -> ValSto
    def eval(self, env, sto):
        return ValSto(ClosureV(self.idx, self.body, env), sto)


# Corresponde a un tipo Expr. Se utiliza para aplicar funciones a argumentos
class AppExpr:
    # Constructor; __init__ : Expr Expr
    def __init__(self, fun_expr, arg_expr):
        self.funExpr = fun_expr
        self.argExpr = arg_expr

    # eval : Env Sto -> ValSto
    def eval(self, env, sto):
        vs_fun_expr = self.funExpr.eval(env, sto)
        sto_fun = vs_fun_expr.get_sto()

        closure = vs_fun_expr.get_val()
        idx = closure.idx
        body = closure.body
        fenv = closure.env

        arg_v_s = self.argExpr.eval(env, sto_fun)
        arg_val = arg_v_s.get_val()
        arg_sto = arg_v_s.get_sto()
        new_loc = next_location(arg_sto)
        extended_env = extend_env(idx, new_loc, fenv)
        extended_sto = extend_sto(new_loc, arg_val, arg_sto)

        return body.eval(extended_env, extended_sto)


# Corresponde a un tipo Expr. Se utiliza para ejecutar una instruccion y
#  seguida de otra con el Store de la primrea heredado, con el fin de recordar mutaciones.
class SeqnExpr:
    # Constructor; __init__ : Expr Expr
    def __init__(self, expr1, expr2):
        self.expr1 = expr1
        self.expr2 = expr2

    # eval : Env Sto -> ValSto
    def eval(self, env, sto):
        vs = self.expr1.eval(env, sto)
        return self.expr2.eval(env, vs.get_sto())


# Corresponde a un tipo Expr. Se utiliza para mutar variables.
class SetExpr:
    # Constructor; __init__ : Symbol Expr
    def __init__(self, idx, valexpr):
        self.idx = idx
        self.valexpr = valexpr

    # eval : Env Sto -> ValSto
    def eval(self, env, sto):
        loc = lookup_env(self.idx.get_string(), env)
        vs = self.valexpr.eval(env, sto)
        return ValSto(vs.get_val(), extend_sto(loc, vs.get_val(), vs.get_sto()))


# Varias expresiones regulares utilizadas para construir otras mas complejas con el objetivo de parsear
# el lenguaje
parenthesesBlock = r"\(.*?\)"
anyItem = r"(" + r"[^\s()]+" + r"|" + parenthesesBlock + r")"  # any number or another block of parentheses
atLeastOneWhiteSpace = r"\s+"
anyWhiteSpaces = r"\s*"
startParentheses = r"^" + anyWhiteSpaces + r"\(" + anyWhiteSpaces
endParentheses = anyWhiteSpaces + r"\)" + anyWhiteSpaces + r"$"


# Construye una expresion regular generica para varios metodos con lo que es posible representar:
# (operator-name args ...)
# Se usa para operadores binarios, if0, app, seqn y set
# opRegex :: String Int -> String
def op_regex(which, argsnum):
    cond_white_space = ""
    if argsnum >= 1:
        args_str = (anyItem + atLeastOneWhiteSpace) * (argsnum-1) + anyItem
        cond_white_space = atLeastOneWhiteSpace
    else:
        args_str = ""

    if which == "":
        cond_white_space = ""

    return startParentheses + which + cond_white_space + args_str + endParentheses


# Expresiones regulares especificas para parsear las S-Expr correspondientes
numRegex = r"(^-?\d+$)"
idRegex = r"(^[^\s()]+$)"
sumRegex = op_regex(r"\+", 2)
diffRegex = op_regex(r"\-", 2)
if0Regex = op_regex(r"\bif0\b", 3)
withRegex = startParentheses + r"\bwith\b" +\
            anyWhiteSpaces + r"\(" + anyWhiteSpaces +\
            r"([^\s]+)" + atLeastOneWhiteSpace + anyItem +\
            anyWhiteSpaces + r"\)" + anyWhiteSpaces + anyItem + endParentheses
funRegex = startParentheses + r"\bfun\b" +\
    anyWhiteSpaces + r"\(" + anyWhiteSpaces + r"([^\s]+)" + anyWhiteSpaces + r"\)" +\
    anyWhiteSpaces + anyItem + endParentheses
# appRegex = op_regex(r"", 2)
appRegex = startParentheses + r"(" + parenthesesBlock + r"|" + r"[^\d][^\s]*" + r")" +\
           anyWhiteSpaces + anyItem + endParentheses
seqnRegex = op_regex(r"\bseqn\b", 2)
setRegex = op_regex(r"\bset\b", 2)


# Parsea un string a una expresion, que esta representada por estructuras de datos (clases-objetos)
# parser :: String (S-Expr) -> Expr
def parser(s):
    num_match = re.search(numRegex, s)
    if num_match:
        return Num(int(num_match.group(1)))

    id_match = re.search(idRegex, s)
    if id_match:
        return IdExpr(id_match.group(1))

    sum_match = re.search(sumRegex, s)

    if sum_match:
        return SumExpr(parser(sum_match.group(1)),
                       parser(sum_match.group(2)))

    diff_match = re.search(diffRegex, s)

    if diff_match:
        return DiffExpr(parser(diff_match.group(1)),
                        parser(diff_match.group(2)))

    if0_match = re.search(if0Regex, s)

    if if0_match:
        return If0Expr(parser(if0_match.group(1)),
                       parser(if0_match.group(2)),
                       parser(if0_match.group(3)))

    with_match = re.search(withRegex, s)
    if with_match:
        return AppExpr(FunExpr(with_match.group(1), parser(with_match.group(3))),
                       parser(with_match.group(2)))

    fun_match = re.search(funRegex, s)
    if fun_match:
        return FunExpr(fun_match.group(1), parser(fun_match.group(2)))

    seqn_match = re.search(seqnRegex, s)
    if seqn_match:
        return SeqnExpr(parser(seqn_match.group(1)), parser(seqn_match.group(2)))

    set_match = re.search(setRegex, s)
    if set_match:
        return SetExpr(parser(set_match.group(1)), parser(set_match.group(2)))

    app_match = re.search(appRegex, s)
    if app_match:
        return AppExpr(parser(app_match.group(1)), parser(app_match.group(2)))

    raise ParsingException(s)


# Interpreta una expresion en un ambiente y en un store, resultando un Value-Store
# interp ::  Expr Env Sto -> ValSto
def interp(expr, env, sto):
    return expr.eval(env, sto)


# Parsea un string y luego lo interpreta
# run :: String (S-expr) -> String
def run(s):
    try:
        parsed = parser(s)
        env = EmptyEnv()
        sto = EmptySto()

        interpreted = interp(parsed, env, sto)
        output = str(interpreted.get_val())
        return output
    except (ParsingException, NotFoundInEnvException, LocNotFoundException) as err:
        return str(err)


"""
print run("(- 3 (+ 5 (+ 0 1000)))")
print run("(   if0   1     1     2     )")
print run("   (with (x 5) (+ x (with (y 3) (+ 10 y))))")
print run("     (       with (      x   (    fun    (     y    ) (    +     123     y   )   ) )  (  x   1      ))   ")
print run("(with (x (fun (y) (+ 123 y)))(x 1))")
print run("     (     with (      x      (fun (x) (+ 123 x))) (x 0))")
#print run("(with (y 3) (+ x y))")
print run("(with (x (fun (x) "
          "(with (y 3) (+ x y) )"
          "))(x 5))")
print run("(fun (x) x)")

print run ("( "
           "( " 
           "(fun (x) x ) "
           "(fun (x) (+ x 5)) "
           ") 3 )")

#print run("(with (x 3) x")
print run("(with (x 3)(with (f (fun (y) (+ x y)))(with (x 5) (f 4))))")
#print run("y")
print run("(with (x 3)(+ (seqn (set x 5) x) x))")
"""
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

#Exceptions
class LocNotFoundException(Exception):
    def __init__(self, msg):
        self.msg = msg

    def __str__(self):
        return "Can't find loc: '" + self.msg + "'"


class NotFoundInEnv(Exception):
    def __init__(self, idx):
        self.idx = idx

    def __str__(self):
        return "error: identificador libre!! " + self.idx


class ParsingError(Exception):
    def __init__(self, s):
        self.s = s

    def __str__(self):
        return "Can't parse string: '" + self.s + "'"


#Environment
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
class EmptyEnv:
    def lookupEnv(self, idx):
        raise NotFoundInEnv(idx)


class Env:
    def __init__(self, idx, value, nextEnv):
        self.idx = idx
        self.value = value
        self.nextEnv = nextEnv

    def lookupEnv(self, idx):
        if self.idx == idx:
            return self.value
        return self.nextEnv.lookupEnv(idx)


def extendEnv(idx, value, env):
    return Env(idx, value, env)


def lookupEnv(idx, env):
    return env.lookupEnv(idx)


#Store
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
    def __init__(self):
        pass

    def lookupSto(self, loc):
        raise LocNotFoundException(loc)

    def nextLocation(self):
        return 0

class Sto:
    def __init__(self, loc, val, sto):
        self.loc = loc
        self.val = val
        self.nextSto = sto
        pass

    def lookupSto(self, loc):
        if loc == self.loc:
            return self.val

        return self.nextSto.lookupSto(loc)

    def nextLocation(self):
        return 1 + self.nextSto.nextLocation()


def lookupSto(loc, sto):
    return sto.lookupSto(loc)


def extendSto(loc, val, sto):
    return Sto(loc, val, sto)


def nextLocation(sto):
    return sto.nextLocation()


"""
ValSto (Value-Store) data structure
<valsto> = (v*s val sto)
"""
class ValSto:
    def __init__(self, val, sto):
        self.val = val
        self.sto = sto

    def getVal(self):
        return self.val

    def getSto(self):
        return self.sto

"""
Value data structures
<Value> ::= <NumV>
        | <ClosureV>
        
Closure for functions:        
<ClosureV> ::= (id body env)

"""
class NumV:
    def __init__(self, num):
        self.num = num

    def getNum(self):
        return self.num()

    def __add__(self, other):
        return NumV(self.num + other.num)

    def __sub__(self, other):
        return NumV(self.num - other.num)

    def __str__(self):
        return str(self.num)


class ClosureV:
    def __init__(self, idx, body, env):
        self.idx = idx
        self.body = body
        self.env = env

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

class Num:
    def __init__(self, num):
        self.num = num

    def eval(self, env, sto):
        return ValSto(NumV(self.num), sto)


class BinaryExpr:
    def __init__(self,  expr1, expr2):
        self.expr1 = expr1
        self.expr2 = expr2


class SumExpr(BinaryExpr):
    def eval(self, env, sto):
        vs1 = self.expr1.eval(env, sto)
        vs2 = self.expr2.eval(env, vs1.getSto())

        return ValSto(vs1.getVal() + vs2.getVal(), vs2.getSto())


class DiffExpr(BinaryExpr):
    def eval(self, env, sto):
        vs1 = self.expr1.eval(env, sto)
        vs2 = self.expr2.eval(env, vs1.getSto())

        return ValSto(vs1.getVal() - vs2.getVal(), vs2.getSto())

class If0Expr:
    def __init__(self, expr1, expr2, expr3):
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3

    def eval(self, env, sto):
        valsto = self.expr1.eval(env, sto)
        if valsto.getVal() == 0:
            return self.expr2.eval(env, sto)
        else:
            return self.expr3.eval(env, sto)

class IdExpr:
    def __init__(self, idx):
        self.idx = idx

    def eval(self, env, sto):
        loc = lookupEnv(self.idx, env)
        return ValSto(lookupSto(loc, sto), sto)

    def getString(self):
        return self.idx


class FunExpr:
    def __init__(self, idx, body):
        self.idx = idx
        self.body = body

    def eval(self, env, sto):
        return ValSto(ClosureV(self.idx, self.body, env), sto)


class AppExpr:
    def __init__(self, funExpr, argExpr):
        self.funExpr = funExpr
        self.argExpr = argExpr

    def eval(self, env, sto):
        vsFunExpr = self.funExpr.eval(env, sto)
        stoFun = vsFunExpr.getSto()

        closure = vsFunExpr.getVal()
        idx = closure.idx
        body = closure.body
        fenv = closure.env

        argVS = self.argExpr.eval(env, stoFun)
        argVal = argVS.getVal()
        argSto = argVS.getSto()
        newLoc = nextLocation(argSto)
        extendedEnv = extendEnv(idx, newLoc, fenv)
        extendedSto = extendSto(newLoc, argVal, argSto)

        return body.eval(extendedEnv, extendedSto)


class SeqnExpr:
    def __init__(self, expr1, expr2):
        self.expr1 = expr1
        self.expr2 = expr2

    def eval(self, env, sto):
        vs = self.expr1.eval(env, sto)
        return self.expr2.eval(env, vs.getSto())


class SetExpr:
    def __init__(self, idx, valexpr):
        self.idx = idx
        self.valexpr = valexpr

    def eval(self, env, sto):
        loc = lookupEnv(self.idx.getString(), env)
        vs = self.valexpr.eval(env, sto)
        return ValSto(vs.getVal(), extendSto(loc, vs.getVal(), vs.getSto()))


parenthesesBlock = r"\(.*?\)"
anyItem = r"(" + r"[^\s()]+" + r"|" + parenthesesBlock + r")"  # any number or another block of parentheses
atLeastOneWhiteSpace = r"\s+"
anyWhiteSpaces = r"\s*"
startParentheses = r"^" + anyWhiteSpaces + r"\(" + anyWhiteSpaces
endParentheses = anyWhiteSpaces + r"\)" + anyWhiteSpaces + r"$"


def opRegex(which, argsnum):
    condWhiteSpace = ""
    if argsnum >= 1:
        args_str = (anyItem + atLeastOneWhiteSpace) * (argsnum-1) + anyItem
        condWhiteSpace = atLeastOneWhiteSpace
    else:
        args_str = ""

    if which == "":
        condWhiteSpace = ""

    return startParentheses + which + condWhiteSpace + args_str + endParentheses


numRegex = r"(^-?\d+$)"
idRegex = r"(^[^\s]+$)"
sumRegex = opRegex(r"\+", 2)
diffRegex = opRegex(r"\-", 2)
if0Regex = opRegex(r"\bif0\b", 3)
withRegex = startParentheses + r"\bwith\b" +\
            anyWhiteSpaces + r"\(" + anyWhiteSpaces +\
            r"([^\s]+)" + atLeastOneWhiteSpace + anyItem +\
            anyWhiteSpaces + r"\)" + anyWhiteSpaces + anyItem + endParentheses
funRegex = startParentheses + r"\bfun\b" +\
    anyWhiteSpaces + r"\(" + anyWhiteSpaces + r"([^\s]+)" + anyWhiteSpaces + r"\)" +\
    anyWhiteSpaces + anyItem + endParentheses
appRegex = opRegex(r"", 2)
seqnRegex = opRegex(r"\bseqn\b", 2)
setRegex = opRegex(r"\bset\b", 2)


"""
parser :: String (S-Expr) -> Expr
"""
def parser(s):
    #print "Parsing string '" + s + "' ..."
    numMatch = re.search(numRegex, s)
    if numMatch:
        #print "numMatch"
        return Num(int(numMatch.group(1)))

    idMatch = re.search(idRegex, s)
    if(idMatch):
        return IdExpr(idMatch.group(1))

    sumMatch = re.search(sumRegex, s)

    if sumMatch:
        #print "sumMatch"
        return SumExpr(parser(sumMatch.group(1)),
                       parser(sumMatch.group(2)))

    diffMatch = re.search(diffRegex, s)

    if diffMatch:
        #print "diffMatch"
        return DiffExpr(parser(diffMatch.group(1)),
                        parser(diffMatch.group(2)))

    if0Match = re.search(if0Regex, s)

    if if0Match:
        #print "if0Match"
        return If0Expr(parser(if0Match.group(1)),
                       parser(if0Match.group(2)),
                       parser(if0Match.group(3)))

    #print "no match"

    withMatch = re.search(withRegex, s)
    if withMatch:
        return AppExpr(FunExpr(withMatch.group(1), parser(withMatch.group(3))),
                       parser(withMatch.group(2)))


    funMatch = re.search(funRegex, s)
    if(funMatch):
        return FunExpr(funMatch.group(1), parser(funMatch.group(2)))

    appMatch = re.search(appRegex, s)
    if(appMatch):
        return AppExpr(parser(appMatch.group(1)), parser(appMatch.group(2)))

    seqnMatch = re.search(seqnRegex, s)
    if(seqnMatch):
        return SeqnExpr(parser(seqnMatch.group(1)), parser(seqnMatch.group(2)))

    setMatch = re.search(setRegex, s)
    if(setMatch):
        return SetExpr(parser(setMatch.group(1)), parser(setMatch.group(2)))


    raise ParsingError(s)

"""
Interpreta una expresion en un ambiente y en un store, resultando un Value-Store
interp ::  Expr Env Sto -> ValSto
"""
def interp(expr, env, sto):
    return expr.eval(env, sto)

"""
Parsea un string y luego lo interpreta
run :: String (S-expr) -> String
"""
def run(s):
    parsed = parser(s)
    env = EmptyEnv()
    sto = EmptySto()
    interpreted = interp(parsed, env, sto)
    output = interpreted.getVal()
    return output

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
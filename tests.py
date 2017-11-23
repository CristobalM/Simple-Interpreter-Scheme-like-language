
import unittest
from simple_interpreter import *


class TestStructures(unittest.TestCase):
    emptyE = EmptyEnv()
    env2 = Env('x', 5, emptyE)
    env3 = Env('y', 6, env2)

    # Tests para code coverage

    def test_lookupEnv(self):
        x = lookup_env('x', self.env3)
        self.assertEqual(x, 5)
        y = lookup_env('y', self.env3)
        self.assertEqual(y, 6)
        with self.assertRaises(NotFoundInEnvException) as context:
            lookup_env('r', self.env3)

        self.assertTrue('error: identificador libre!! r' in context.exception.__str__())

    def test_extendEnv(self):
        env4 = extend_env('z', 9, self.env3)
        z = lookup_env('z', env4)
        self.assertEqual(z, 9)
        env5 = extend_env('w', 14, env4)
        z = lookup_env('z', env5)
        self.assertEqual(z, 9)
        w = lookup_env('w', env5)
        self.assertEqual(w, 14)

    def test_sto(self):  # lookup, extend, nextLocation tests
        empty_sto = EmptySto()

        loc1 = next_location(empty_sto)
        sto1 = extend_sto(loc1, 1, empty_sto)
        loc2 = next_location(sto1)
        sto2 = extend_sto(loc2, 2, sto1)

        self.assertEqual(loc1, 0)
        self.assertEqual(loc2, 1)

        val1 = lookup_sto(0, sto2)
        val2 = lookup_sto(1, sto2)
        self.assertEqual(val1, 1)
        self.assertEqual(val2, 2)
        with self.assertRaises(LocNotFoundException) as context:
            lookup_sto(10, sto2)

        self.assertTrue("Can't find loc: '10'" in context.exception.__str__())

    def test_val_sto(self):
        sto = EmptySto()
        v = NumV(5)
        sto2 = extend_sto(next_location(sto), v, sto)
        vs = ValSto(v, EmptySto())
        vs2 = lookup_sto(0, sto2)
        self.assertEqual(vs.get_val().get_num(),
                         vs2.get_num())

        sto_got = vs.get_sto()
        self.assertEqual(sto_got.next_location(), 0)

    def test_num_v(self):
        a = NumV(1)
        b = NumV(5)
        c = a + b
        self.assertEqual(6, c.get_num())
        d = a - b
        self.assertEqual(-4, d.get_num())

        str_t = str(c)

        self.assertEqual("6", str_t)

    def test_closure_v(self):
        env = extend_env('x', 5, EmptyEnv())
        cv = ClosureV('x', SumExpr(Num(1), Num(2)), env)
        self.assertEqual(str(cv), "Function")

    def test_num(self):
        a = Num(5)
        vs = a.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 5)

    def test_sum_expr(self):
        a = SumExpr(Num(1), Num(2))
        vs = a.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 3)

    def test_diff_expr(self):
        a = DiffExpr(Num(1), Num(2))
        vs = a.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), -1)

    def test_if0_expr(self):
        a = If0Expr(Num(0), Num(1), Num(2))
        vs = a.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 1)

        b = If0Expr(Num(1), Num(1), Num(2))
        vs2 = b.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs2.get_val().get_num(), 2)

    def test_id_expr(self):
        sym = 'x'
        empty_sto = EmptySto()
        loc = next_location(empty_sto)
        sto = extend_sto(loc, NumV(5), empty_sto)

        a = IdExpr(sym)
        env = extend_env(sym, loc, EmptyEnv())

        vs = a.eval(env, sto)
        self.assertEqual(vs.get_val().get_num(), 5)
        self.assertEqual(a.get_string(), sym)

    def test_fun_expr(self):
        sym = 'x'
        a = FunExpr(sym, SumExpr(Num(5), Num(6)))
        vs = a.eval(EmptyEnv(), EmptySto())
        self.assertEqual(str(vs.get_val()), "Function")

    def test_app_expr(self):
        sym = 'x'
        a = FunExpr(sym, SumExpr(Num(5), Num(6)))
        b = Num(5)
        c = AppExpr(a, b)
        vs = c.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 11)

    def test_seqn_expr(self):
        a = SumExpr(Num(5), Num(6))
        b = SumExpr(Num(1), Num(2))
        c = SeqnExpr(a, b)
        vs = c.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 3)

    def test_set_expr(self):
        sym = 'x'
        a = AppExpr(FunExpr(sym,
                            SeqnExpr(SetExpr(IdExpr(sym), Num(5)), IdExpr(sym))),
                    Num(6))

        vs = a.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 5)


class TestParser(unittest.TestCase):
    def test_num_match(self):
        s = "5"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 5)

    def test_id_match(self):
        s = "x"
        e = parser(s)
        self.assertEqual(e.get_string(), s)

    def test_sum_match(self):
        s = "(+ 5 3)"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 8)

    def test_diff_match(self):
        s = "(- 5 3)"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 2)

    def test_if0_match(self):
        s = "(if0 0 1 2)"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 1)

        s = "(if0 1 1 2)"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 2)

    def test_with_match(self):
        s = "(with (x 5) x)"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 5)

    def test_fun_match(self):
        s = "(fun (x) x)"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(str(vs.get_val()), "Function")

    def test_app_match(self):
        s = "((fun (x) (+ x 1)) 5)"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 6)

    def test_seqn_match(self):
        s = "(seqn 1 2)"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 2)

    def test_set_match(self):
        s = "(with (x 5) (seqn (set x 1) x))"
        e = parser(s)
        vs = e.eval(EmptyEnv(), EmptySto())
        self.assertEqual(vs.get_val().get_num(), 1)

    def test_error(self):
        s = "((("
        err = 'error: expresion invalida ' + s

        with self.assertRaises(ParsingException) as context:
            parser(s)

        self.assertTrue(err in context.exception.__str__())

        s = "(8 10)"
        err = 'error: expresion invalida ' + s

        with self.assertRaises(ParsingException) as context:
            parser(s)

        self.assertTrue(err in context.exception.__str__())


class TestInterp(unittest.TestCase):
    def test_1(self):  # No hay mas tests dada la simplicidad de interp y delega su funcionalidad
        expr = SumExpr(Num(1), Num(2))
        env = EmptyEnv()
        sto = EmptySto()
        vs = interp(expr, env, sto)
        self.assertEqual(vs.get_val().get_num(), 3)


class TestRun(unittest.TestCase):
    def test_1(self):
        r = run("y")
        self.assertEqual(r, "error: identificador libre!! y")

    def test_2(self):
        r = run("(fun (x) x)")
        self.assertEqual(r, "Function")

    def test_3(self):
        r = run("(8 10)")
        self.assertEqual(r, "error: expresion invalida (8 10)")

    def test_4(self):
        r = run("(((fun (x) x)"
                " (fun (x) (+ x 5)))"
                " 3)")
        self.assertEqual(r, "8")

    def test_5(self):
        r = run("(with (x 3)"
                " (with (f (fun (y) (+ x y)))"
                " (with (x 5)"
                " (f 4))))")

        self.assertEqual(r, "7")

    def test_6(self):
        r = run("(with (x 3)"
                "(+ (seqn (set x 5) x) x))")

        self.assertEqual(r, "10")

    # Tests adicionales
    def test_7(self):
        s = "(with (x 3)" \
            "(with (f (fun (y) (+ x y)))" \
            "(with (x 5)" \
            "(seqn (set x 1) (with (y 8) (+ (f y) (- 1 x)))) )))"
        r = run(s)
        self.assertEqual(r, "11")

    def test_8(self):
        s = "(with (x 1) (seqn (set x (+ 1 x)) x))"
        r = run(s)
        self.assertEqual(r, "2")

    def test_9(self):
        s = "(with (x (if0 (- 1 1) 2 3)) (seqn (set x (+ 1 x)) x))"
        r = run(s)
        self.assertEqual(r, "3")


if __name__ == '__main__':
    unittest.main()

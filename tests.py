import unittest
from tarea3 import *


class Test1(unittest.TestCase):
    def test_extendenv(self):
        emptyE = EmptyEnv()
        env2 = Env('x', 5, emptyE)
        env3 = Env('y', 6, env2)
        x = lookupEnv('x', env3)
        self.assertEqual(x, 5)
        y = lookupEnv('y', env3)
        self.assertEqual(y, 6)


if __name__ == '__main__':
    unittest.main()
from unittest import TestCase

from lye.ast import Drums
from lye.visitors.maps import DrumsTransformer

class TestDrumsTransformer(TestCase):

    def test_simplify_drums(self):
        ast = Drums(expr="test")
        result = DrumsTransformer().visit(ast)
        self.assertEqual(result, "test")

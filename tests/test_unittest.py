import cpppy as cpp
import pytest
import warnings
@pytest.fixture
def rules():
    rules = cpp.rules(qpp=True)
    return rules

def pending():
    warnings.warn(PendingDeprecationWarning("This method will be removed in future versions.  Use 'tree.iter()' or 'list(tree.iter())' instead. for elem in self.tree.iter() if Element_has_iter else self.tree.getiterator():"))
    return 1
##@pytest.fixture
#def
ympe_value = [(1966, 5000),
              (1980, 13100),
              (2025, 68539),
              (2060, 192858)]
@pytest.mark.filterwarnings("ignore:PendingDeprecationWarning")
class TestCPP:

    #def test_load_rules(self):
    #    yrspars =cpp.load_rules()
     #   assert type(yrspars) == cpp.np.recarray

    def test_init_rules(self,rules):       
        assert isinstance(rules,cpp.rules)
    
    #def test_rules_loc(self,rules): 
    #    assert  rules.loc(2010) == 44
    
    @pytest.mark.parametrize( "year, value",ympe_value)                           
    def test_rules_ympe(self, rules, year,value):
        assert  rules.ympe(year) == pytest.approx(value,0.5)

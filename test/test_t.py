from cpp import cpp
import pytest

@pytest.fixture
def rules():
    rules = cpp.rules(cpp.load_rules())
    return rules
##@pytest.fixture
#def
ympe_value = [(1966, 5000),
              (1980, 13100),
              (2025, 68539),
              (2060, 192858)]
class TestCPP:

    def test_load_rules(self):
        yrspars =cpp.load_rules()
        assert type(yrspars) == cpp.np.recarray

    def test_init_rules(self,rules):       
        assert isinstance(rules,cpp.rules)
    
    def test_rules_loc(self,rules): 
        assert  rules.loc(2010) == 44
    
    @pytest.mark.parametrize( "year, value",ympe_value)                           
    def test_rules_ympe(self, rules, year,value):
        assert  rules.ympe(year) == pytest.approx(value,0.5)

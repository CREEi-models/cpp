import srpp
import pytest

@pytest.fixture
def rules():
    rules = srp.rules(qpp=True)
    return rules

ympe_value = [(1966, 5000),
              (1980, 13100),
              (2025, 68539),
              (2060, 177549.94)]
benefit_value = [(64,64,1,2020,1092.54),
                (65,65,1,2020,1177.30),
                (66,66,1,2020,1276.20)]

class Test_unittest:
    def test_init_rules(self,rules):  
        assert isinstance(rules,srp.rules)

    @pytest.mark.parametrize( "year, value",ympe_value)  
    def test_rules_ympe(self,rules,year,value):
        assert  rules.ympe(year) == pytest.approx(value,0.5)

class Test_integration:
    @pytest.mark.parametrize("claimage, retage, ratio, year, benefit",benefit_value)  
    def test_benefit(self,rules,claimage,retage,ratio,year,benefit):
        case = srp.account(year-claimage,rules=rules)
        case.SetHistory_ratio(retage=retage,a={"r":1.1,'n':retage-18})
        case.SetHistory_fam(claimage=claimage,age_birth=[])
        case.RunCase(claimage=claimage)
        assert case.gBenefit(year) == pytest.approx(benefit,5)

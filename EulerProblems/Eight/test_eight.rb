class TestFindGreatestProduct

  def test_product_of_all_digits
    s = ('5'..'9').to_a.join
    n = FindGreatestProduct.new Cleaner.new(s).string, s.length
    k = [5,6,7,8,9].inject &:*
    assert_equal k, n
  end

end

class TestCleaner
  GOOD_STRING = ('0'..'9').to_a.join

  def test_junk_inserted
    g = GOOD_STRING.dup
    bad = [0,3].zip([' ',"\n"]).each{|i,s| g.insert i, s}
    assert GOOD_STRING==Cleaner.new(bad).string
  end
  
end


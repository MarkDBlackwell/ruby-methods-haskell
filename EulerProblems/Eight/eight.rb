=begin
Author: Mark D. Blackwell
Dates:
(mdb) September 23, 2011 - created

Part of my learning Haskell.
Ruby versions: 1.8.7.
Haskell version: 2010.2.0.0

Found at: http://fedner.net/blog/2010/04/06/euler-problem-8/
Euler's problem #8
Find the greatest product of five consecutive digits in the 1000-digit number.
The large number was copied from the above website.
=end

class FindGreatestProduct

  def initialize string, n_consecutive_digits
    digits = string.chars.map &:to_i
    digit_runs = digits.each_cons n_consecutive_digits
    products = digit_runs.map{|a| a.inject &:*}
    @max = products.max
    @indices = (0...products.length).select{|i| products.at(i)==@max}
    @max_digits = digit_runs.to_a.values_at *@indices
  end

  def to_s
    [@max.to_s, @indices.inspect, @max_digits.inspect].join "\n"
  end

end

class Cleaner
  attr_reader :string
  
  REGEXP = Regexp.new '\D+', Regexp::MULTILINE # One or more non-digits.
  EMPTY_STRING = ''

  def initialize s
    @string = s.gsub REGEXP, EMPTY_STRING
  end

  def length
    @string.length
  end
end

class BigString
  def self.value
    Cleaner.new(DATA.readlines.join).string
  end
end

N_CONSECUTIVE_DIGITS = 5
print FindGreatestProduct.new(BigString.value, N_CONSECUTIVE_DIGITS).to_s

__END__
    73167176531330624919225119674426574742355349194934
    96983520312774506326239578318016984801869478851843
    85861560789112949495459501737958331952853208805511
    12540698747158523863050715693290963295227443043557
    66896648950445244523161731856403098711121722383113
    62229893423380308135336276614282806444486645238749
    30358907296290491560440772390713810515859307960866
    70172427121883998797908792274921901699720888093776
    65727333001053367881220235421809751254540594752243
    52584907711670556013604839586446706324415722155397
    53697817977846174064955149290862569321978468622482
    83972241375657056057490261407972968652414535100474
    82166370484403199890008895243450658541227588666881
    16427171479924442928230863465674813919123162824586
    17866458359124566529476545682848912883142607690042
    24219022671055626321111109370544217506941658960408
    07198403850962455444362981230987879927244284909188
    84580156166097919133875499200524063689912560717606
    05886116467109405077541002256983155200055935729725
    71636269561882670428252483600823257530420752963450

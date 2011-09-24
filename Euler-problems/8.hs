findGreatestProduct string n_consecutive_digits = maximum products

digits = map (to_i) string
digitRuns = eachCons digits nConsecutiveDigits
products = mapWith (*) digitRuns
max = maximum products
indices = findIndices (==max) products
maxDigits = map (!!) digitRuns indices
to_s = intersperse '\n' [max, indices, maxDigits]

using DataFrames, Distributions, RDatasets

iris = dataset("datasets","iris")

function svySampling(DataFrame::AbstractDataFrame,SampleSize::Int)
	## Initialization of variables ##
	μ = DiscreteUniform(1,nrow(DataFrame))
	A = Set()
	for i in 1:nrow(DataFrame)
		Randᵤ= rand(μ,1)
		if in(Randᵤ,A)


	end
end

function surveydesign(DataFrame,)
end


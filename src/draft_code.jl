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

function svySRS(DataFrame::AbstractDataFrame, SampleSize::Int)
	## Procedure: Random sort SRSWOR, based in Algorith 4.5 from Yves Tillé book.
	## For each of the DataFrame rows, we generate a random value from a [0,1] Uniform distribution.
	## After this, we sort the DataFrame and select the desired sample size (based in SampleSize value).
	μ = Uniform()
	Rand = rand(μ,nrow(DataFrame))
	DataFrame₂= hcat(DataFrame,Rand)
	sort!(DataFrame₂,:x1,rev=true)
	return DataFrame₂[1:SampleSize,:]

	## How to put logs here?
end
	
svySRS(iris,20)



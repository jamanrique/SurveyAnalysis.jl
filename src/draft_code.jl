using DataFrames, Distributions, RDatasets

iris = dataset("datasets","iris")

## Creation of SampleObject. Special attention to SampleStrat. This has to connect with something right? Sampling -> Doc. 
## Notes to self: Follow Kish notation.

struct SampleObject
	N::Int
	n::Int
	SampleStrat::String
	Sample::AbstractDataFrame
    function SampleObject(args...)

        new(N, n, SampleStrat, Sample)
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
	DataFrame₂ = DataFrame₂[1:SampleSize,:]
	## How to put logs here?	
	return SampleObject(nrow(DataFrame),nrow(DataFrame₂),"SRSWOR",DataFrame₂)
end

test=svySRS(iris,30)
typeof(test)
test.N

function svyReservoirSRS(DataFrame::AbstractDataFrame,SampleSize::Int)
	## Procedure: Reservoir method for SRSWOR
	## Select the first n units, then for the remaining values
	## with probability n/k (Bernoulli) select this unit and replace it in the selected sample. This replacement
	## is at random.
	DataFrame₂ = first(DataFrame,SampleSize)
	β = Bernoulli(SampleSize/nrow(DataFrame))
	μ = DiscreteUniform(1,SampleSize)
	DataFrameₖ = last(DataFrame,nrow(DataFrame)-SampleSize)
	for row in eachrow(DataFrameₖ)
		Randᵦ = rand(β,1)
		if Randᵦ==[1]
			Randᵤ= rand(μ,1)
			DataFrame₂[Randᵤ,:] = DataFrame(row) ## current problem: can't replace row! broadcasting issues.
		end
	end
	return SampleObject(nrow(DataFrame),nrow(DataFrame₂),"SRSWOR",DataFrame₂)
end

svyReservoirSRS(iris,20)


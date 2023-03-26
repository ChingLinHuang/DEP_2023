using Pkg
Pkg.activate("C:\\Users\\andy\\.julia\\packages")

using Distributions
using Plots
using LinearAlgebra
using GaussianRandomFields
using Distances
using DataFrames
using CSV
using StatsBase
using DataStructures
using TableView # showtable function

reps = 10
reps = 1

pwd()
cd("P:\\Personal\\Andy\\plthompson-Meta_com_framework-36ca456_ver1")

first_record = 0
@progress for rep = 1:reps
    println(rep)
    #-----------------------------------------
    ## read data
    #----------------------------------------
    # environment setting of patches
    env_df = CSV.read("./data/landscape_data/env_$rep.csv", DataFrame)
    # emigration probabilities
    disp_mat = CSV.read("./data/landscape_data/disp_mat_$rep.csv", DataFrame)
    disp_mat = Matrix(disp_mat)
    disp_mat = disp_mat[:, 2:end]
    # coordinates of patches
    landscape = CSV.read("./data/landscape_data/landscape_$rep.csv", DataFrame)
    # Tmax and burn-in time
    time = CSV.read("./data/landscape_data/time_$rep.csv", DataFrame)

    #-----------------------------------------------------
    ## model parameters setting
    #-----------------------------------------------------
    # species number
    S = 50
    # number of patches
    M = size(disp_mat)[1]
    # Tmax
    burn_in = time.burn_in[1]
    generations = maximum(env_df.time) - burn_in
    Tmax = burn_in + generations
    # environment matrix: col = patches, row = time, value = env1
    env_mat2 = zeros(maximum(env_df.time), M)
    for i = 1:maximum(env_df.time)
        env_mat2[i, :] = env_df.env1[env_df.time.==i]
    end

    # indices of loop
    # k: scenarios of niche optima
    # l: scenarios of niche width
    # m: shape of niche

    for k = 1:4
        println(k)
        for l = 1:6
            println(l)
            for m = 1
                # basic fitness
                r = 5

                # species optima/ trait
                if (k == 1) # uniform distribution
                    z = reshape(repeat(rand(Uniform(0,1),S), outer = [M]), S, M)'
                elseif (k == 2) # unimodel
                    z = reshape(repeat(rand(Beta(5,5),S), outer = [M]), S, M)'
                elseif (k == 3) # slightly right skewed
                    z = reshape(repeat(rand(Beta(2.5,5),S), outer = [M]), S, M)'
                else  (k == 4) # strongly right skewed
                    z = reshape(repeat(rand(Beta(5,1.5),S), outer = [M]), S, M)'
                end

                # species niche breadth
                if (l == 1) # uniform distribution
                    σ_niche_V = range(0.001, stop = 10, length = S)
                elseif (l == 2) # unimodel
                    σ_niche_V = sort(rand(Beta(5,5),S)) * 10 .+ 0.001
                elseif (l == 3) # slightly right skewed
                    σ_niche_V = range(0.001^(1/1.5), stop = 10^(1/1.5), length = S).^1.5
                elseif (l == 4) # strongly right skewed
                    σ_niche_V = range(0.001^0.5, stop = 10^0.5, length = S).^2
                elseif (l == 5) # slightly left skewed
                    σ_niche_V = range(0.001^1.2, stop = 10^1.2, length = S).^(1/1.2)
                else # strongly left skewed
                    σ_niche_V = range(0.001^1.5, stop = 10^1.5, length = S).^(1/1.5)
                end
                σ_niche = reshape(repeat(σ_niche_V, inner = [M]), M, S)

                # competition parameter (stable competition)
                α = 0.5
                α = rand(Uniform(0, α), S, S) # intra > inter
                α[diagind(α)] = repeat([1.0], outer = S)
                α = α * 0.05

                # dispersal probability (slightly limited dispersal)
                a = 0.0001

                global Model_df
                # initial species composition
                N = rand(Poisson(0.5), M, S) * 1.0
                # recruitment events
                seedV = convert(
                    Array{Int64},
                    collect(burn_in/(10):burn_in/(10):burn_in/2),
                )
                # record times
                sampV = Tmax
                #-------------------------------------
                # model
                @progress for gen = 1:Tmax
                    global x

                    if (any(y -> y == gen, seedV)) # recruitment event
                        N = N + rand(Poisson(0.5), M, S) * 1.0
                    end

                    x = reshape(repeat(env_mat2[gen, :], outer = [S]), M, S) # env

                    # fitness
                    if (m == 1)
                        env = exp.(-((x - z) ./ (2.0 * σ_niche)) .^ 2.0)
                    end

                    den = N * α

                    λ_v = r * N .* (1.0 ./ (1.0 .+ den)) .* env

                    λ_v[λ_v.<0.0] .= 0.0

                    N = [rand(Poisson(λ)) for λ in λ_v]

                    emigrants = [rand(Binomial(n, a)) for n in N]
                    immigrants_exp = disp_mat * emigrants #expected number of immigrants
                    immigrants_S = sum(emigrants, dims = 1) #number of immigrants per species
                    immigrants = zeros(M, S)
                    for l = 1:S
                        immigrants[:, l] =
                            collect(
                                values(
                                    SortedDict(
                                        countmap(
                                            [
                                                1:M
                                                wsample(
                                                    1:M,
                                                    immigrants_exp[:, l] /
                                                    sum(
                                                        immigrants_exp[
                                                            :,
                                                            l,
                                                        ],
                                                    ),
                                                immigrants_S[l],
                                                )
                                            ],
                                        ),
                                    ),
                                ),
                            ) .- 1
                    end
                    sum(sum(emigrants, dims = 1) .== sum(immigrants, dims = 1))
                    N = N .- emigrants .+ immigrants
                    N[N.<0.0] .= 0.0
                end

                Model_df_1 = DataFrame(
                    N = N[:],
                    Species = repeat(1:S, inner = M),
                    optima = repeat(z[1, :], inner = M),
                    width = repeat(σ_niche_V, inner = M),
                    Patch = repeat(1:M, outer = S),
                    env_cal = repeat(x[:,1], outer = S),
                    k = k,
                    l = l,
                    m = m,
                    rep = rep
                )

                Model_df_1 = Model_df_1[Model_df_1[:, :N].>0, :]
                if (first_record == 0)
                    Model_df = Model_df_1
                    first_record = 1
                else
                    Model_df = [Model_df; Model_df_1]
                end
            end
        end
    end
end
CSV.write("C:/Users/andy/Downloads/Beta_20210704/outputfile.csv", Model_df)

# 20210604
# Change a_write to number. Make sure no error when reading in R
# Write the file for every rep and k to let the size of the data not too big
# every species has different niche width

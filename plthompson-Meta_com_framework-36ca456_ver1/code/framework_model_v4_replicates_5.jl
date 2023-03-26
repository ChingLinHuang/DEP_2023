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

reps = 30
# same setting of environment and species traits

pwd()
cd("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1")
#-----------------------------------------
## read data
#----------------------------------------
# environment setting of patches
env_df = CSV.read("./data/landscape_data/env_1.csv", DataFrame)
# emigration probabilities
disp_mat = CSV.read("./data/landscape_data/disp_mat_1.csv", DataFrame)
disp_mat = Matrix(disp_mat)
disp_mat = disp_mat[:, 2:end]
# coordinates of patches
landscape = CSV.read("./data/landscape_data/landscape_1.csv", DataFrame)
# Tmax and burn-in time
time = CSV.read("./data/landscape_data/time_1.csv", DataFrame)

#-----------------------------------------------------
## model parameters setting
#-----------------------------------------------------
# species number
S = 50
# dominant species (for patch dynamics scenario)
dominants = trunc(Int, round(S * 0.3))
# number of patches
M = size(disp_mat)[1]
# Tmax
burn_in = time.burn_in[1]
generations = maximum(env_df.time) - burn_in
Tmax = burn_in + generations
# environment matrix: col = patches, row = time, value = fixed env
env_mat2 = zeros(maximum(env_df.time), M)
for i = 1:maximum(env_df.time)
    env_mat2[i, :] = env_df.env2[env_df.time.==i]
end

# basic fitness
r = 5
# species optima/ trait
z = reshape(repeat(rand(S), outer = [M]), S, M)'
# species niche breadth
σ_niche_V = exp.(range(log(0.001), stop = log(10), length = 13))
# competition scenarios
α_V = [0,0.5,1.5]
# dispersal probability
a_Vect = exp.(range(log(1e-5), stop = log(1), length = 16))
a_Vect = a_Vect[a_Vect.<1]
    


# model
for rep = 20
    println("rep = $rep")
    @time for k = 1:(length(α_V)+2)
        println("k = $k")
        if (k <= length(α_V))
            if (α_V[k] == 0)
                α = zeros(S, S) * 1.0 # no inter-competition
                α_write = string(0)
            elseif (α_V[k] == 0.5)
                α = rand(Uniform(0, α_V[k]), S, S) # intra > inter
                α_write = string(0.5)
            else (α_V[k] == 1.5)
                α = rand(Uniform(1, α_V[k]), S, S) # intra < inter
                α_write = string(1.5)
            end
        elseif (k == (length(α_V) + 1))
            α = ones(S, S) * 1.0 # intra = inter
            α_write = string(1) # equal -> 1
        else
            α = rand(Uniform(0, 1), S, S) # patch_dynamics
            α_hold = rand(Uniform(0, 1), S, S)
            α[1:dominants, :] = rand(Uniform(1, 1.5), dominants, S)
            α[LowerTriangular(α).>0] = α_hold[LowerTriangular(α).>0]
            α_write = string("patch") # patch_dynamics -> 7
        end
        α[diagind(α)] = repeat([1.0], outer = S)
        α = α * 0.05

        for i = 1:length(a_Vect)
            a = a_Vect[i] # parameter of binomial for emigrats
            for j = 1:length(σ_niche_V)
                σ_niche = σ_niche_V[j]

                # initial species composition
                N = rand(Poisson(0.5), M, S)
                # recruitment events
                seedV = convert(
                    Array{Int64},
                    collect(burn_in/(10):burn_in/(10):burn_in/2),
                )
                # sampling times (only sample 60 times)
                sampV = convert(Array{Int64}, collect((burn_in+800):20:Tmax))

                # results preparation
                N_save = N
                env_save = z

                #-------------------------------------
                # model
                for gen = 1:Tmax
                    if (any(y -> y == gen, seedV)) # recruitment event
                        N = N + rand(Poisson(0.5), M, S)
                    end

                    x = reshape(repeat(env_mat2[gen, :], outer = [S]), M, S) # env

                    env = exp.(-((x - z) ./ (2.0 * σ_niche)) .^ 2.0) # fitness

                    den = N * α

                    λ_v = r * N .* (1.0 ./ (1.0 .+ den)) .* env

                    λ_v[λ_v.<0.0] .= 0.0

                    N = [rand(Poisson(λ)) for λ in λ_v]

                    if k < (length(α_V) + 2)
                        emigrants = [rand(Binomial(n, a)) for n in N]
                        immigrants_exp = disp_mat * emigrants #expected number of immigrants
                        immigrants_S = sum(emigrants, dims = 1) #number of immigrants per species
                        immigrants = zeros(M, S)
                        for ll = 1:S
                            immigrants[:, ll] =
                                collect(
                                    values(
                                        SortedDict(
                                            countmap(
                                                [
                                                    1:M
                                                    wsample(
                                                        1:M,
                                                        immigrants_exp[:, ll] /
                                                        sum(
                                                            immigrants_exp[
                                                                :,
                                                                ll,
                                                            ],
                                                        ),
                                                    immigrants_S[ll],
                                                )
                                            ],
                                        ),
                                    ),
                                ),
                            ) .- 1
                        end
                    else
                        emigrants = [rand(Binomial(n, a)) for n in N]
                        emigrants[:, 1:dominants] = [
                            rand(Binomial(n, a * 0.1)) for
                            n in N[:, 1:dominants]
                        ] # weak colonizer
                        immigrants_exp = disp_mat * emigrants #expected number of immigrants
                        immigrants_S = sum(emigrants, dims = 1) #number of immigrants per species
                        immigrants = zeros(M, S)
                        for ll = 1:S
                            immigrants[:, ll] =
                                collect(
                                    values(
                                        SortedDict(
                                            countmap(
                                                [
                                                    1:M
                                                    wsample(
                                                        1:M,
                                                        immigrants_exp[:, ll] /
                                                        sum(
                                                            immigrants_exp[
                                                                :,
                                                                ll,
                                                            ],
                                                        ),
                                                        immigrants_S[ll],
                                                    )
                                                ],
                                            ),
                                        ),
                                    ),
                                ) .- 1
                        end
                    end
                    N[rand(Binomial(1, 0.002), M, S).>0] .= 0 # disturbance
                    #sum(sum(emigrants, dims = 1) .== sum(immigrants, dims = 1))
                    N = N .- emigrants .+ immigrants
                    N[N.<0.0] .= 0.0

                    if (any(y -> y == gen, sampV))
                        N_save = cat(dims = 3, N_save, N)
                        env_save = cat(dims = 3, env_save, x)
                    end
                end

                N_save = N_save[:, :, 2:end]
                env_save = env_save[:, :, 2:end]

                Model_df_1 = DataFrame(
                    rep = rep,
                    N = N_save[:],
                    env = env_save[:],
                    Species = repeat(1:S, inner = M, outer = length(sampV)),
                    Time = repeat(1:length(sampV), inner = S * M),
                    Patch = repeat(1:M, outer = length(sampV) * S),
                    z = repeat(z[1, :], inner = M, outer = length(sampV)),
                    dispersal = a,
                    sig_niche = σ_niche,
                    alpha = string(α_write)
                )

                Model_df_1 = Model_df_1[Model_df_1[:, :N].>0, :]

                #Model_df = [Model_df; Model_df_1]

                CSV.write("./outputs/replicates/outputfile_rep$rep" * "k$k" * "i$i" * "j$j" * ".csv", Model_df_1)
            end
        end
    end
end

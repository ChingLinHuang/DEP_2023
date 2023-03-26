
Pkg.activate("C:\\Users\\andy\\.julia\\packages")
using DataFrames
using CSV
using Distances
using Statistics
using LinearAlgebra
using Random
using Dates
using StatsBase
using DataStructures

# Function bMNTD
function bMNTD(spe, dist_trait, sp_ind)
    # initial matrix for bmntd
    bmntd = zeros((size(spe, 1), size(spe,1)))

    for k = 1:(size(spe, 1)-1)
        for m = (k+1):size(spe, 1)
            # site data without absent species
            site_k = spe[k, sp_ind[k,:]]
            site_m = spe[m, sp_ind[m,:]]

            # subset the distance matrix with occurred species
            dist_trait_km = dist_trait[sp_ind[k,:], sp_ind[m,:]]

            min_km = vec(minimum(dist_trait_km, dims = 2)) # minimum for each row
            min_mk = vec(minimum(dist_trait_km, dims = 1)) # minimum for each col

            # Calculate βMNTD by average of the inner product
            bmntd[k,m] = 0.5*(site_k'min_km + site_m'min_mk)
        end
    end

    return bmntd
end

# Function bNTI
function bNTI(spe, trait, n_perm)
    # trait distance matrix of species
    dist_trait = pairwise(Euclidean(), trait'; dims = 2)
    # index matrix of occurrence of species in each plot
    sp_ind = spe .> 0

    # observed βMNTD
    bmntd_obs = bMNTD(spe, dist_trait, sp_ind)

    # Initial matrix for null model βMNTD
    bmntd_null = zeros((size(spe, 1), size(spe, 1), n_perm + 1))
    bmntd_null[:,:,1] = bmntd_obs
    for i = 2:(n_perm + 1)
        ind = shuffle(1:size(trait,1))
        dist_trait_perm = dist_trait[ind,ind]
        bmntd_null[:,:,i] = bMNTD(spe, dist_trait_perm, sp_ind)
    end

    bmntd_mean = zeros((size(spe, 1), size(spe, 1)))
    bmntd_sd = zeros((size(spe, 1), size(spe, 1)))
    for k = 1:(size(spe, 1)-1)
        for m = (k+1):size(spe, 1)
            bmntd_mean[k,m] = mean(bmntd_null[k,m,:])
            bmntd_sd[k,m] = std(bmntd_null[k,m,:])

            if (bmntd_mean[k,m] == 0) & (bmntd_sd[k,m] == 0)
                bmntd_sd[k,m] = 1
            end
        end
    end
    bnti = (bmntd_obs .- bmntd_mean)./bmntd_sd
    return bnti
end

# Function RC_BRAY
function RC_BRAY(spe, n_perm)
    # Bray-Curtis distance matrix of species compostition
    BC_obs = pairwise(BrayCurtis(), spe', dims = 2)

    # initial null model Bray-Curtis distance matrix
    BC_null = zeros((size(spe, 1), size(spe, 1), n_perm + 1))
    BC_null[:,:,1] = BC_obs

    spe_inc = spe .> 0 # incident data
    gamma = vec(sum(spe_inc, dims = 2)) # number of species in each patch
    abundance = vec(sum(spe, dims = 1)) # abundance of each species in whole metacommunity
    spe_perm = copy(spe)

    for i = 2:(n_perm+1)
        # Permuation with fixed number of species and total abundance for each row
        for ind = 1:size(spe,1)
            # Decide the occurred species
            spe_perm[ind,:] = sample(spe_inc[ind,:], size(spe_perm,2), replace = false)

            # Define the probatility to occur by the relative abundance of the speceis which are chosen in the first step
            abund_prob = abundance .* spe_perm[ind,:]
            # randomly generated the abundance of the species by the relative abundance
            # Total abundance and number of species in each plot are preserved
            sp_list = countmap(wsample(1:size(spe_perm,2),
                                        abund_prob / sum(abund_prob),
                                        sum(spe[ind,:]) - gamma[ind]))
            sp_list = hcat(collect(keys(sp_list)), collect(values(sp_list)))
            spe_perm[ind,sp_list[:,1]] = spe_perm[ind,sp_list[:,1]] + sp_list[:,2]
        end
        BC_null[:,:,i] = pairwise(BrayCurtis(), spe_perm', dims = 2)
    end

    BC_null_temp = BC_null ./ ((maximum(BC_null, dims = 3) .- minimum(BC_null, dims = 3))) .*2
    BC_null_temp = BC_null_temp .- maximum(BC_null_temp, dims = 3) .+ 1

    RC_bray = BC_null_temp[:,:,1]
    return(RC_bray)
end


cd("C:\\Users\\andy\\Downloads\\analysis")
for rep = 15:18
    iter = 1
    dat_stegen = DataFrame(
        rep = zeros(5*15*13*4),
        k = zeros(5*15*13*4),
        i = zeros(5*15*13*4),
        j = zeros(5*15*13*4),
        Time = zeros(5*15*13*4),
        Selection = zeros(5*15*13*4),
        DispLimit = zeros(5*15*13*4),
        HomoDisp = zeros(5*15*13*4),
        Drift = zeros(5*15*13*4)
    )
    @time for k = 1:5
        for i = 1:15
            for j = 1:13
                for t = [41 42 43 44]
                    println("rep$rep"*"k$k"*"i$i"*"j$j"*"t$t")
                    ### Read data
                    dat_trait = CSV.read(".\\data\\trait_4_3\\trait_rep$rep"*"k$k"*"i$i"*"j$j"*"t$t"*".csv", DataFrame)
                    dat_spe = CSV.read(".\\data\\spe_4_3\\spe_rep$rep"*"k$k"*"i$i"*"j$j"*"t$t"*".csv", DataFrame)

                    trait = dat_trait[:,2]
                    dat_spe = dat_spe[:,2:ncol(dat_spe)]
                    spe = Matrix(dat_spe)
                    if size(spe, 2) < 3
                        dat_stegen[iter, 1:5] = [rep k i j t]
                        iter = iter + 1
                        continue
                    elseif (sum(spe .> 0) < 200)  | (sum(spe) < 1000)
                        dat_stegen[iter, 1:5] = [rep k i j t]
                        iter = iter + 1
                        continue
                    end

                    # convert the relative abundance
                    spe_frac = spe ./ sum(eachcol(spe))


                    ### Stegen's framework
                    # Step 1
                    bnti = bNTI(spe_frac, trait, 99)
                    # Step 2
                    RC_bray_raw = RC_BRAY(spe, 99)


                    # indices where need to calculate RC_bray
                    ind_step2 = (bnti .<= 2) .* (bnti .>= -2)
                    RC_bray = RC_bray_raw .* ind_step2

                    # Write out the results
                    #cd("C:\\Users\\andy\\Downloads\\analysis\\stegen")
                    #CSV.write("./outputs/bnti_rep$rep" * "k$k" * "i$i" * "j$j" * ".csv", bnti)
                    #CSV.write("./outputs/RC_bray_rep$rep" * "k$k" * "i$i" * "j$j" * ".csv", BC_bray)

                    # Relative importance of processes
                    N_combi = size(spe, 1) * (size(spe, 1) - 1) / 2
                    Selection = sum((bnti .> 2) .| (bnti .< -2)) / N_combi
                    DispLimit = sum((RC_bray_raw[ind_step2] .> 0.95)) / N_combi
                    HomoDisp = sum((RC_bray_raw[ind_step2] .< -0.95)) / N_combi
                    Drift = sum((RC_bray_raw[ind_step2] .<= 0.95) .* (RC_bray_raw[ind_step2] .>= -0.95)) / N_combi
                    # Selection + DispLimit + HomoDisp + Drift == 1
                    #sum((bnti .> 2) .| (bnti .< -2))
                    #sum((bnti .<= 2) .& (bnti .>= -2))
                    #sum((RC_bray_raw[ind_step2] .<= 1.001) .| (RC_bray_raw[ind_step2] .>= -1.001))


                    dat_stegen_1 = DataFrame(
                        rep = rep,
                        k = k,
                        i = i,
                        j = j,
                        Time = t,
                        Selection = Selection,
                        DispLimit = DispLimit,
                        HomoDisp = HomoDisp,
                        Drift = Drift
                    )

                    dat_stegen[iter,:] = dat_stegen_1[1,:]
                    iter = iter + 1


                end
            end
        end
    end

    CSV.write("./res/Stegen_4/outputfile_rep$rep"*"_3_1.csv", dat_stegen)
end

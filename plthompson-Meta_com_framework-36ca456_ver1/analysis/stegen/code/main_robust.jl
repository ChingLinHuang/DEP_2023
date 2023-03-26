

cd("C:\\Users\\andy\\Downloads\\analysis")

dat_stegen = DataFrame(
    rep = zeros(3240), # 6
    t = zeros(3240), # 4
    archetype = "SS", # 4 -> 1
    effort = zeros(3240), # 9
    n_sample = zeros(3240), # 15
    Selection = zeros(3240),
    DispLimit = zeros(3240),
    HomoDisp = zeros(3240),
    Drift = zeros(3240)
)

iter = 1
for rep = 13:18
    @time for archetype = "SS"
        for effort = 1:9
            for n_sample = 1:15
                for t = [60 56 52 48]
                    println("rep$rep"*"effort$effort"*"n_sample$n_sample"*"t$t")
                    ### Read data
                    dat_trait = CSV.read(".\\data\\trait_4_3_robust\\rep$rep"*"_$archetype"*"_effort$effort"*"n$n_sample"*"t$t"*".csv", DataFrame)
                    dat_spe = CSV.read(".\\data\\spe_4_3_robust\\rep$rep"*"_$archetype"*"_effort$effort"*"n$n_sample"*"t$t"*".csv", DataFrame)

                    trait = dat_trait[:,2]
                    dat_spe = dat_spe[:,2:ncol(dat_spe)]
                    spe = Matrix(dat_spe)

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
                        t = t,
                        archetype = archetype,
                        effort = effort,
                        n_sample = n_sample,
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
end

CSV.write("./res/outputfile_robust_1.csv", dat_stegen)



cd("C:\\Users\\andy\\Downloads\\analysis")
for rep = 1:6
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
                for t = [60 56 52 48]
                    println("rep$rep"*"k$k"*"i$i"*"j$j"*"t$t")
                    ### Read data
                    dat_trait = CSV.read(".\\data\\trait_4_1\\trait_rep$rep"*"k$k"*"i$i"*"j$j"*"t$t"*".csv", DataFrame)
                    dat_spe = CSV.read(".\\data\\spe_4_1\\spe_rep$rep"*"k$k"*"i$i"*"j$j"*"t$t"*".csv", DataFrame)

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

    CSV.write("./res/Stegen_4/outputfile_rep$rep.csv", dat_stegen)
end

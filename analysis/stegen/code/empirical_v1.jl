
dat_stegen = DataFrame(
    Time = 1:4,
    Selection = zeros(4),
    DispLimit = zeros(4),
    HomoDisp = zeros(4),
    Drift = zeros(4)
)


for t = 1:4

    cd("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\data")
    ### Read data
    dist_trait = CSV.read("dist_trait_$t.csv", DataFrame)
    dat_spe = CSV.read("spe_$t.csv", DataFrame)

    dist_trait  = dist_trait[:,2:ncol(dist_trait)]
    dist_trait = Matrix(dist_trait)
    dat_spe = dat_spe[:,2:ncol(dat_spe)]
    spe = Matrix(dat_spe)

    # convert the relative abundance
    spe_frac = spe ./ sum(eachcol(spe))


    ### Stegen's framework
    # Step 1
    bnti = bNTI_1(spe_frac, dist_trait, 99)
    # Step 2
    RC_bray_raw = RC_BRAY(spe, 99)

    # indices where need to calculate RC_bray
    ind_step2 = (bnti .<= 2) .* (bnti .>= -2)
    # RC_bray = RC_bray_raw .* ind_step2

    # Write out the results
    #cd("C:\\Users\\andy\\Downloads\\analysis\\stegen")
    #CSV.write("./outputs/bnti_rep$rep" * "k$k" * "i$i" * "j$j" * ".csv", bnti)
    #CSV.write("./outputs/RC_bray_rep$rep" * "k$k" * "i$i" * "j$j" * ".csv", BC_bray)

    # Relative importance of processes
    N_combi = size(spe, 1) * (size(spe, 1) - 1) / 2

    dat_stegen_1 = DataFrame(
    Time = t,
    Selection = sum((bnti .> 2) .| (bnti .< -2)) / N_combi,
    DispLimit = sum((RC_bray_raw[ind_step2] .> 0.95)) / N_combi,
    HomoDisp = sum((RC_bray_raw[ind_step2] .< -0.95)) / N_combi,
    Drift = sum((RC_bray_raw[ind_step2] .<= 0.95) .* (RC_bray_raw[ind_step2] .>= -0.95)) / N_combi
    )

    dat_stegen[t,:] = dat_stegen_1[1,:]

end


CSV.write("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res_stegen.csv", dat_stegen)
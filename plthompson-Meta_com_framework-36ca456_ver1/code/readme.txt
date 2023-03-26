## frame_model
framework_model.jl: original setting
framework_model_v1.jl: original setting with code cleaning
framework_model_v2.jl: 
	1. the code with wider parameter space for searching for the suitable parameters
	2. dispersal parameter "a" is changed from controlling the probability parameter in binomial distribution to controlling the scale of the emigrants. The probability of the parameter in binomial distribution is set to be 0.2.
	3. Add new parameter "b" which quantifies the ability to disperse far. Larger "b", weeker far dispersal ability. So dispersal contain two parameter "a" and "b" which are representing the number of emigrants and ability to disperse far, respectively. (disp_mat = disp_mat.^b * Diagonal(1 ./ vec(sum(disp_mat.^b, dims = 2)))). For the CC-Trade off scenario the emigrants number of weak colonizers are set by rand(Binomial(n, 0.01)) compare to the strong colonizers rand(Binomial(n, 0.2)). 
	4. lower bound of s_niche is 0.001, otherwise it will shut down because of two low env (fitness).
framework_model_v2_1.jl: 
	1. try to do the parallel calculation but I give up
framework_model_v2_2.jl: 
	1. Prepare for the "nasty" parallel calculation
framework_model_v2_3.jl:
	1. s_niche_V = exp.(range(log(0.001), stop = log(1000), length = 8))
	2. change emigrants = [rand(Binomial(n, 0.05)) for n in N] * a
	3. grow_V = 1:4:9

## generate_landscapes
generate_landscapes.R: original setting
generate_landscapes_v1.R: four env scenarios with different var and scale in rmexp()
generate_landscapes_v2.R: clean original setting and add the plotting for the environment

## model_functions
model_functions_v1.R: clean the code





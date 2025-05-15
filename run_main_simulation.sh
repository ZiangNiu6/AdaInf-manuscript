# write activate renv
Rscript -e 'renv::activate(); renv::restore()'

# 1. Define the epsilon greedy list
eps_list=(0.1 0.2 0.4)
distribution_list=("gaussian" "Bernoulli")
algorithm_list=("thompson" "eps_greedy")
directory=simulation-code/main-simulation

# 2. Simulation
# loop over eps_list
for eps in "${eps_list[@]}"
do
    # jump to next iteration if the file already exists
    eps_id=$(echo "$eps * 100 / 1" | bc)
    
    # loop over distribution
    for distribution in "${distribution_list[@]}"
    do    
        # extract the output directory 
        output_dir="$directory/$distribution"
        mkdir -p $output_dir
        
        for algorithm in "${algorithm_list[@]}"
        do 
            # skip the simulation if is there
            if [ -f "$output_dir/${algorithm}_eps_${eps_id}_results.rds" ]; then
                echo "Results already exist for $algorithm $distribution simulation with eps=$eps. Skipping."
                continue
            fi
    
            # Submit the job for the current eps
            echo "Rscript $directory/simulation-${distribution}.R $eps $algorithm $output_dir" | qsub -N "${algorithm}_${distribution}_simulation_eps_${eps}" -l m_mem_free=12G
        
            echo "Submitted jobs for $algorithm $distribution alternative simulation with $eps greedy algorithm!"
        done
    done
done


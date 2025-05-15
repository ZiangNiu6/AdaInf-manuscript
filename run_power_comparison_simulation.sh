# write activate renv
Rscript -e 'renv::activate(); renv::restore()'

# 1. Define the epsilon greedy list
distribution_list=("gaussian" "Bernoulli" "mix_normal" "Poisson" "Student")
signal_list=(0.00 0.01 0.02 0.03 0.04)
directory=simulation-code/power-comparison

# 2. Simulation
# loop over 5 distributions
for distribution in "${distribution_list[@]}"
do    
    # extract the output directory 
    output_dir="$directory/$distribution"
    mkdir -p $output_dir
    
    for signal in "${signal_list[@]}"
    do 
        # skip the simulation if is there
        if [ -f "$output_dir/${distribution}_${signal}_power_comparison.rds" ]; then
            echo "Results already exist for $distribution power compairson with signal $signal. Skipping."
            continue
        fi
    
        # Submit the job for the current eps
        echo "Rscript $directory/power_comparison.R $distribution $signal $output_dir" | qsub -N "${distribution}_${signal}_power_comparison_simulation" -l m_mem_free=12G
        
        echo "Submitted jobs for $distribution power comparison for signal $signal with Thompson sampling algorithm!"
    done
done


#include "utilities.h"

/* =============================================================================
 * Inserts the new traits into the offspring matrix for diploids
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     offspr:          The row of the offspring that is being added
 * ========================================================================== */
void insert_diploid_traits(double **offspring, double *paras, int offspr){
    
    int i, j, k, vec_pos, row, col, layer, loci, traits, layers, net_vals;
    int loci_col, trait_col, layer_col, trait_st, net_st, loci_st, net1_st;
    double L1, L2, **loc_layer, ***net, **net_sum, **loci_to_traits, **L, **T;
    
    loci_col   = (int) paras[11];  /* Column where the number of loci is held */
    trait_col  = (int) paras[12];  /* Column where the number of traits held  */
    layer_col  = (int) paras[13];  /* Column where network layer number held  */

    loci     = (int) offspring[offspr][loci_col];
    traits   = (int) offspring[offspr][trait_col];
    layers   = (int) offspring[offspr][layer_col];

    trait_st  = (int) paras[59];      /* Column where the trait columns start */
    net_st    = trait_st + traits;    /* Column where net locations start     */
    loci_st   = net_st + layers + 3;  /* Col where first loci values start    */
    net_vals  = (loci * traits) + (layers * traits * traits);
    
    T  = malloc(1 * sizeof(double *));
    for(row = 0; row < 1; row++){
        T[row] = malloc(traits * sizeof(double));   
    }

    L  = malloc(1 * sizeof(double *));
    for(row = 0; row < 1; row++){
        L[row] = malloc(loci * sizeof(double));   
    }

    loc_layer  = malloc(loci * sizeof(double *));
    for(row = 0; row < loci; row++){
        loc_layer[row] = malloc(traits * sizeof(double));   
    }

    net   = malloc(layers * sizeof(double *));
    for(k = 0; k < layers; k++){
        net[k] = malloc(traits * sizeof(double *));
        for(i = 0; i < traits; i++){
            net[k][i] = malloc(traits * sizeof(double));   
        }
    }

    net_sum = malloc(traits * sizeof(double *));
    for(row = 0; row < traits; row++){
        net_sum[row] = malloc(traits * sizeof(double));   
    } 

    loci_to_traits  = malloc(loci * sizeof(double *));
    for(row = 0; row < loci; row++){
        loci_to_traits[row] = malloc(traits * sizeof(double));   
    } 
    
    vec_pos = loci_st;
    for(i = 0; i < loci; i++){
        L1      = offspring[offspr][vec_pos];
        L2      = offspring[offspr][vec_pos + loci];
        L[0][i] = L1 + L2;  
        vec_pos++;
    }

    vec_pos = loci_st + (2 * loci);
    for(row = 0; row < loci; row++){
        for(col = 0; col < traits; col++){
            L1                  = offspring[offspr][vec_pos];
            L2                  = offspring[offspr][vec_pos + net_vals];
            loc_layer[row][col] = L1 + L2;
            vec_pos++;
        }
    }

    for(layer = 0; layer < layers; layer++){
        for(row = 0; row < traits; row++){
            for(col = 0; col < traits; col++){
                L1                   = offspring[offspr][vec_pos];
                L2                   = offspring[offspr][vec_pos + net_vals];
                net[layer][row][col] = L1 + L2;
                vec_pos++;
            }
        }
    }
    
    matrix_zeros(traits, traits, net_sum);
    matrix_zeros(loci, traits, loci_to_traits);

    /* Gets the summed effects of network by multiplying matrices */
    sum_network_layers(traits, layers, net, net_sum);

    /* Matrix that gets the final phenotype from the genotype */
    matrix_multiply(loc_layer, net_sum, loci, traits, traits, traits,
                    loci_to_traits);

    /* Multiply the loci vector by the loci_to_traits matrix to get traits */
    matrix_multiply(L, loci_to_traits, 1, loci, loci, traits, T);

    vec_pos = trait_st; /* Now put the traits back to offspring row */
    for(i = 0; i < traits; i++){
        offspring[offspr][vec_pos] = T[0][i];
        vec_pos++;
    }

    for(row = 0; row < loci; row++){
        free(loci_to_traits[row]);
    }
    free(loci_to_traits);

    for(row = 0; row < traits; row++){
        free(net_sum[row]);
    }
    free(net_sum);

    for(row = 0; row < loci; row++){
        free(loc_layer[row]);
    }
    free(loc_layer);

    for(k = 0; k < layers; k++){
        for(i = 0; i < traits; i++){
            free(net[k][i]);
        }
        free(net[k]);        
    }
    free(net); 

    for(row = 0; row < 1; row++){
        free(L[row]);
    }
    free(L);

    for(row = 0; row < 1; row++){
        free(T[row]);
    }
    free(T);
}

/* =============================================================================
 * Inserts the new traits into the offspring matrix for haploids
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     offspr:          The row of the offspring that is being added
 * ========================================================================== */
void insert_haploid_traits(double **offspring, double *paras, int offspr){
    
    int i, j, k, vec_pos, row, col, layer, loci, traits, layers;
    int loci_col, trait_col, layer_col, trait_st, net_st, loci_st, net1_st;
    double **loc_layer, ***net, **net_sum, **loci_to_traits, **L, **T;

    loci_col   = (int) paras[11];  /* Column where the number of loci is held */
    trait_col  = (int) paras[12];  /* Column where the number of traits held  */
    layer_col  = (int) paras[13];  /* Column where network layer number held  */
    
    loci     = (int) offspring[offspr][loci_col];
    traits   = (int) offspring[offspr][trait_col];
    layers   = (int) offspring[offspr][layer_col];
    
    trait_st  = (int) paras[59];      /* Column where the trait columns start */
    net_st    = trait_st + traits;    /* Column where net locations start     */
    loci_st   = net_st + layers + 2;  /* Col where first loci values start    */
    
    T  = malloc(1 * sizeof(double *));
    for(row = 0; row < 1; row++){
        T[row] = malloc(traits * sizeof(double));   
    }
    
    L  = malloc(1 * sizeof(double *));
    for(row = 0; row < 1; row++){
        L[row] = malloc(loci * sizeof(double));   
    }
    
    loc_layer  = malloc(loci * sizeof(double *));
    for(row = 0; row < loci; row++){
        loc_layer[row] = malloc(traits * sizeof(double));   
    }
    
    net   = malloc(layers * sizeof(double *));
    for(k = 0; k < layers; k++){
        net[k] = malloc(traits * sizeof(double *));
        for(i = 0; i < traits; i++){
            net[k][i] = malloc(traits * sizeof(double));   
        }
    }
    
    net_sum = malloc(traits * sizeof(double *));
    for(row = 0; row < traits; row++){
        net_sum[row] = malloc(traits * sizeof(double));   
    } 
    
    loci_to_traits  = malloc(loci * sizeof(double *));
    for(row = 0; row < loci; row++){
        loci_to_traits[row] = malloc(traits * sizeof(double));   
    } 
    
    vec_pos = loci_st;
    for(i = 0; i < loci; i++){
        L[0][i] = offspring[offspr][vec_pos];  
        vec_pos++;
    }
    
    for(row = 0; row < loci; row++){
        for(col = 0; col < traits; col++){
            loc_layer[row][col] = offspring[offspr][vec_pos];
            vec_pos++;
        }
    }
    
    for(layer = 0; layer < layers; layer++){
        for(row = 0; row < traits; row++){
            for(col = 0; col < traits; col++){
                net[layer][row][col] = offspring[offspr][vec_pos];
                vec_pos++;
            }
        }
    }
    
    matrix_zeros(traits, traits, net_sum);
    matrix_zeros(loci, traits, loci_to_traits);
    
    /* Gets the summed effects of network by multiplying matrices */
    sum_network_layers(traits, layers, net, net_sum);

    /* Matrix that gets the final phenotype from the genotype */
    matrix_multiply(loc_layer, net_sum, loci, traits, traits, traits,
                    loci_to_traits);
    
    /* Multiply the loci vector by the loci_to_traits matrix to get traits */
    matrix_multiply(L, loci_to_traits, 1, loci, loci, traits, T);

    vec_pos = trait_st; /* Now put the traits back to offspring row */
    for(i = 0; i < traits; i++){
        offspring[offspr][vec_pos] = T[0][i];
        vec_pos++;
    }
    
    for(row = 0; row < loci; row++){
        free(loci_to_traits[row]);
    }
    free(loci_to_traits);
    
    for(row = 0; row < traits; row++){
        free(net_sum[row]);
    }
    free(net_sum);
    
    for(row = 0; row < loci; row++){
        free(loc_layer[row]);
    }
    free(loc_layer);
    
    for(k = 0; k < layers; k++){
        for(i = 0; i < traits; i++){
            free(net[k][i]);
        }
        free(net[k]);        
    }
    free(net); 
    
    for(row = 0; row < 1; row++){
        free(L[row]);
    }
    free(L);
    
    for(row = 0; row < 1; row++){
        free(T[row]);
    }
    free(T);
}

/* =============================================================================
 * Mutates allele values in the haploid genome with a specific probability
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     offspr:          The row of the offspring that is being added
 * ========================================================================== */
void mutation_haploid(double **offspring, double *paras, int offspr){
    
    int i, j, sex_col, sex, loci, traits, layers, neutrals;
    int loci_col, trait_col, layer_col, neut_col, cols;
    int mutation_type, mutation_layers, trait_st, net_st, loci_st, neut_st;
    int net1_st, net_st_mu, net_ed_mu, base_start;
    int network_st, network_end, network_len, off_col;
    double mu, mu_effect, mu_sd, mutate;
    
    sex_col = (int) paras[4];
    sex     = (int) offspring[offspr][sex_col];
    
    loci_col   = (int) paras[11];  /* Column where the number of loci is held */
    trait_col  = (int) paras[12];  /* Column where the number of traits held  */
    layer_col  = (int) paras[13];  /* Column where network layer number held  */
    neut_col   = (int) paras[29];  /* Column where N neutral alleles held     */
    cols       = (int) paras[57];  /* Number of cols in the offspring array   */

    mutation_type   = (int) paras[61];
    mutation_layers = (int) paras[63];
    mu              = paras[62];
    mu_effect       = paras[64];
    mu_sd           = paras[65];

    loci     = (int) offspring[offspr][loci_col];
    traits   = (int) offspring[offspr][trait_col];
    layers   = (int) offspring[offspr][layer_col];
    neutrals = (int) offspring[offspr][neut_col];

    trait_st  = (int) paras[59];      /* Column where the trait columns start */
    net_st    = trait_st + traits;    /* Column where net locations start     */
    loci_st   = net_st + layers + 2;  /* Col where first loci values start    */
    net1_st   = offspring[offspr][net_st] - 1; /* Minus 1 switches R to C     */
    neut_st   = offspring[offspr][loci_st - 1];

    if(mutation_layers > (layers + 1) ){
        mutation_layers = layers + 1;
    }

    /* Mutate the actual loci */
    for(i = loci_st; i < net1_st; i++){
        mutate = runif(0, 1);
        if(mutate < mu){
            if(mutation_type == 0){
                offspring[offspr][i]  = rnorm(mu_effect, mu_sd);
            }
            if(mutation_type == 1){
                offspring[offspr][i] += rnorm(mu_effect, mu_sd);
            }
        }
    }

    /* Mutate the neutral loci */
    for(i = neut_st; i < cols; i++){
        mutate = runif(0, 1);
        if(mutate < mu){
            if(mutation_type == 0){
                offspring[offspr][i]  = rnorm(mu_effect, mu_sd);
            }
            if(mutation_type == 1){
                offspring[offspr][i] += rnorm(mu_effect, mu_sd);
            }
        }
    }

    /* Mutate the network layers of interest */
    if(mutation_layers > 0){
        base_start  = (int) paras[67];
        network_st  = offspring[offspr][net_st] - 1;
        network_end = offspring[offspr][net_st + layers + 1] - 1;
        network_len = network_end - network_st;
        if(base_start > 0){
            net_st_mu = network_st;
            net_ed_mu = offspring[offspr][net_st + mutation_layers] - 1;
        }else{
            off_col   = net_st + (layers + 1 - mutation_layers);
            net_st_mu = offspring[offspr][off_col] - 1;
            net_ed_mu = network_end;
        }
        for(i = net_st_mu; i < net_ed_mu; i++){
            mutate = runif(0, 1);
            if(mutate < mu){
                if(mutation_type == 0){
                    offspring[offspr][i]  = rnorm(mu_effect, mu_sd);
                }
                if(mutation_type == 1){
                    offspring[offspr][i] += rnorm(mu_effect, mu_sd);
                }
            }
        }
    }
    
}


/* =============================================================================
 * Mutates allele values in the diploid genome with a specific probability
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     offspr:          The row of the offspring that is being added
 * ========================================================================== */
void mutation_diploid(double **offspring, double *paras, int offspr){
    
    int i, j, loci, traits, layers, neutrals;
    int loci_col, trait_col, layer_col, neut_col, cols;
    int mutation_type, mutation_layers, trait_st, net_st, loci_st, neut_st;
    int net1_st, net1_ed, net_st_mu, net_ed_mu, base_start;
    int network_st, network_end, network_len, off_col;
    double mu, mu_effect, mu_sd, mutate;
    
    loci_col   = (int) paras[11];  /* Column where the number of loci is held */
    trait_col  = (int) paras[12];  /* Column where the number of traits held  */
    layer_col  = (int) paras[13];  /* Column where network layer number held  */
    neut_col   = (int) paras[29];  /* Column where N neutral alleles held     */
    cols       = (int) paras[57];  /* Number of cols in the offspring array   */
    
    mutation_type   = (int) paras[61];
    mutation_layers = (int) paras[63];
    mu              = paras[62];
    mu_effect       = paras[64];
    mu_sd           = paras[66];
    
    loci     = (int) offspring[offspr][loci_col];
    traits   = (int) offspring[offspr][trait_col];
    layers   = (int) offspring[offspr][layer_col];
    neutrals = (int) offspring[offspr][neut_col];

    trait_st  = (int) paras[59];      /* Column where the trait columns start */
    net_st    = trait_st + traits;    /* Column where net locations start     */
    loci_st   = net_st + layers + 3;  /* Col where first loci values start    */
    net1_st   = offspring[offspr][net_st] - 1; /* Minus 1 switches R to C     */
    net1_ed   = offspring[offspr][net_st + layers + 1] - 1; /* Minus 1 too    */
    neut_st   = offspring[offspr][loci_st - 1];
    
    if(mutation_layers > (layers + 1) ){
        mutation_layers = layers + 1;
    }
    
    /* Mutate the actual loci */
    for(i = loci_st; i < net1_st; i++){
        mutate = runif(0, 1);
        if(mutate < mu){
            if(mutation_type == 0){
                offspring[offspr][i]  = rnorm(mu_effect, mu_sd);
            }
            if(mutation_type == 1){
                offspring[offspr][i] += rnorm(mu_effect, mu_sd);
            }
        }
    }
    
    /* Mutate the neutral loci */
    for(i = neut_st; i < cols; i++){
        mutate = runif(0, 1);
        if(mutate < mu){
            if(mutation_type == 0){
                offspring[offspr][i]  = rnorm(mu_effect, mu_sd);
            }
            if(mutation_type == 1){
                offspring[offspr][i] += rnorm(mu_effect, mu_sd);
            }
        }
    }
    
    /* Mutate the network layers of interest */
    if(mutation_layers > 0){
        base_start  = (int) paras[67];
        network_st  = offspring[offspr][net_st] - 1;
        network_end = offspring[offspr][net_st + layers + 1] - 1;
        network_len = network_end - network_st;
        if(base_start > 0){
            net_st_mu = network_st;
            net_ed_mu = offspring[offspr][net_st + mutation_layers] - 1;
        }else{
            off_col   = net_st + (layers + 1 - mutation_layers);
            net_st_mu = offspring[offspr][off_col] - 1;
            net_ed_mu = network_end;
        }
        for(i = net_st_mu; i < net_ed_mu; i++){
            mutate = runif(0, 1);
            if(mutate < mu){
                if(mutation_type == 0){
                    offspring[offspr][i]  = rnorm(mu_effect, mu_sd);
                }
                if(mutation_type == 1){
                    offspring[offspr][i] += rnorm(mu_effect, mu_sd);
                }
            }
        }
        for(i = (net_st_mu + network_len); i < (net_ed_mu + network_len); i++){
            mutate = runif(0, 1);
            if(mutate < mu){
                if(mutation_type == 0){
                    offspring[offspr][i]  = rnorm(mu_effect, mu_sd);
                }
                if(mutation_type == 1){
                    offspring[offspr][i] += rnorm(mu_effect, mu_sd);
                }
            }
        }
    }
}

/* =============================================================================
 * Adds sexual individuals into the population given parent info
 *     pests:           The array holding the parent's information
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     ind:             The row of the mother of the individual
 *     offspr:          The row of the offspring that is being added
 * ========================================================================== */
void sire_genes(double **pests, double *paras, double **offspring, int offspr){
    
    int i, j, sire_row, dame_row, traits, layers, loci;
    int mrow_col, srow_col, trait_col, layer_col, loci_col;
    int neut_col, neutrals, loci1_st, loci2_st, sire_chrome;
    int trait_st, net_st, net1_st, net2_st, neut1_st, neut2_st, sire_loc;
    int crossed, net_geno, dame_loc, from_sire, from_dame;
    double crossover, crossit;
    
    mrow_col  = (int) paras[8];   /* Column where mum's row is held          */
    srow_col  = (int) paras[9];   /* Column where the sire's row is held     */ 
    loci_col  = (int) paras[11];  /* Column where the number of loci is held */
    trait_col = (int) paras[12];  /* Column where the number of traits held  */
    layer_col = (int) paras[13];  /* Column where network layer number held  */
    neut_col  = (int) paras[29];  /* Column where N neutral alleles held     */
    crossover = paras[60];        /* Probability that crossover occurs       */
 
    sire_row = (int) offspring[offspr][srow_col];
    dame_row = (int) offspring[offspr][mrow_col];
    loci     = (int) offspring[offspr][loci_col];
    traits   = (int) offspring[offspr][trait_col];
    layers   = (int) offspring[offspr][layer_col];
    neutrals = (int) offspring[offspr][neut_col];
    
    trait_st  = (int) paras[59];        /* Col where the trait columns start  */
    net_st    = trait_st + traits;      /* Col where net locations start      */
    loci1_st  = net_st + layers + 3;    /* Col where first loci values start  */
    loci2_st  = loci1_st + loci;        /* Col where second loci values start */
    net1_st   = loci1_st + (2 * loci);  /* Col where first network starts     */
    net2_st   = net_st + layers;        /* Col where second network starts    */
    neut1_st  = offspring[offspr][loci1_st - 1]; /* Where first neutrals      */
    neut2_st  = neut1_st + neutrals;    /* Col where second neutrals start    */
    net_geno  = net2_st - net1_st;      /* Number of elements in 1 network    */

    sire_chrome = get_rand_int(0, 1);
    from_sire   = get_rand_int(0, 1);
    from_dame   = get_rand_int(0, 1);
    for(i = loci1_st; i < loci2_st; i++){
        crossed  = sire_chrome;
        crossit  = runif(0, 1);
        if(crossit < crossover){
            crossed = (sire_chrome - 1) * (sire_chrome - 1);
        }
        sire_loc = loci * from_sire;
        dame_loc = loci * from_dame;
        if(crossed < 1){
            offspring[offspr][i]        = pests[sire_row][i + sire_loc];
            offspring[offspr][i + loci] = pests[dame_row][i + dame_loc];
        }else{
            offspring[offspr][i + loci] = pests[sire_row][i + sire_loc];
            offspring[offspr][i]        = pests[dame_row][i + dame_loc];
        }
    }
    for(i = net1_st; i < net2_st; i++){
        crossed  = sire_chrome;
        crossit  = runif(0, 1);
        if(crossit < crossover){
            crossed = (sire_chrome - 1) * (sire_chrome - 1);
        }
        sire_loc = net_geno * from_sire;
        dame_loc = net_geno * from_dame;
        if(crossed < 1){
            offspring[offspr][i]            = pests[sire_row][i + sire_loc];
            offspring[offspr][i + net_geno] = pests[dame_row][i + dame_loc];
        }else{
            offspring[offspr][i + net_geno] = pests[sire_row][i + sire_loc];
            offspring[offspr][i]            = pests[dame_row][i + dame_loc];
        }
    }
    for(i = neut1_st; i < neut2_st; i++){
        crossed  = sire_chrome;
        crossit  = runif(0, 1);
        if(crossit < crossover){
            crossed = (sire_chrome - 1) * (sire_chrome - 1);
        }
        sire_loc = neutrals * from_sire;
        dame_loc = neutrals * from_dame;
        if(crossed < 1){
            offspring[offspr][i]            = pests[sire_row][i + sire_loc];
            offspring[offspr][i + neutrals] = pests[dame_row][i + dame_loc];
        }else{
            offspring[offspr][i + neutrals] = pests[sire_row][i + sire_loc];
            offspring[offspr][i]            = pests[dame_row][i + dame_loc];
        }
    }
}

/* =============================================================================
 * Assigns the offspring to a sire based
 *     pests:           The array holding the parent's information
 *     paras:           The paras vector that holds global information
 *     ind:             The row of the mother of the individual
 * ========================================================================== */
int assign_sire(double **pests, double *paras, int ind){
    
    int i, N, in_range, opp_sex, range;
    int mate_col, sex_col, range_col, self_col;
    int nearby_mates, mate_pos, mate_row, mate_sex, focal_sex, selfing;
    
    sex_col   = (int) paras[4];  /* Column where the sex of individual is     */
    mate_col  = (int) paras[27]; /* Column for number of mates accessible     */
    range_col = (int) paras[24];
    self_col  = (int) paras[26];
    
    N            = (int) paras[51];
    nearby_mates = (int) pests[ind][mate_col];
    focal_sex    = (int) pests[ind][sex_col];
    
    range        = pests[ind][range_col];
    selfing      = pests[ind][self_col];
    
    mate_sex     = 1;
    if(focal_sex == 2){
        mate_sex = 3;
    }
    /* Quickly get a random avail sire: replace with function for mate choice */
    mate_pos = 1;
    if(nearby_mates > 1){
        mate_pos = get_rand_int(1, nearby_mates);
    }
    while(N > 0 && mate_pos > 0){
        N--;
        in_range = is_in_range(pests, ind, N, paras, range);
        opp_sex  = pests[N][sex_col];
        if(in_range > 0 && opp_sex == mate_sex){
            if(N != ind || selfing > 0){
                mate_pos--;
            }
        }
        
    }
    
    return N; /* The while loop above finds the row of the mate */
}

/* =============================================================================
 * Adds sexual individuals into the population given parent info
 *     pests:           The array holding the parent's information
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     ind:             The row of the mother of the individual
 *     offspring_count: The row of the offspring that is being added
 * ========================================================================== */
void add_sexual(double **pests, double **offspring, double *paras, int ind,
                int offspring_count){
    
    int trait, cols, ID, sire_row, sire_ID, srow_col, sID_col, sex_col;
    int ID_col, age_col, mID_col, mrow_col, off_col, food_col, pest_col;
    int tag1_col, tag2_col, tag3_col, mate_col;
    
    cols     = (int) paras[57];  /* Columns in the pest array             */
    ID_col   = (int) paras[0];   /* Column where the ID is held           */
    age_col  = (int) paras[3];   /* Column where Age is held              */
    mID_col  = (int) paras[6];   /* Column where mum's ID is held         */
    mrow_col = (int) paras[8];   /* Column where mum's row is held        */
    off_col  = (int) paras[10];  /* Column where offspring number is held */
    food_col = (int) paras[14];  /* Column where food intake is held      */
    pest_col = (int) paras[15];  /* Column where pesticide intake is held */
    tag1_col = (int) paras[20];  /* Column where tag 1 is held            */
    tag2_col = (int) paras[21];  /* Column where tag 2 is held            */
    tag3_col = (int) paras[22];  /* Column where tag 3 is held            */
    mate_col = (int) paras[27];  /* Column where mate accessed is held    */
    srow_col = (int) paras[9];   /* Column where the sire's row is held   */ 
    sID_col  = (int) paras[7];   /* Column where the sire's ID is held    */
    sex_col  = (int) paras[4];   /* Column where the sex is located       */
    
    for(trait = 0; trait < cols; trait++){
        offspring[offspring_count][trait] = pests[ind][trait];
    }

    offspring[offspring_count][ID_col]    = paras[58] + 1.0; 
    offspring[offspring_count][age_col]   = 0;              
    offspring[offspring_count][mID_col]   = pests[ind][0]; 
    offspring[offspring_count][mrow_col]  = ind;            
    offspring[offspring_count][off_col]   = 0;              
    offspring[offspring_count][food_col]  = 0;            
    offspring[offspring_count][pest_col]  = 0; 
    offspring[offspring_count][tag1_col]  = 0; 
    offspring[offspring_count][tag2_col]  = 0; 
    offspring[offspring_count][tag3_col]  = 0; 
    offspring[offspring_count][mate_col]  = 0;
    offspring[offspring_count][sex_col]   = pests[ind][sex_col];
    if(pests[ind][sex_col] > 1){
        offspring[offspring_count][sex_col] = get_rand_int(2, 3);
    }
    
    sire_row = assign_sire(pests, paras, ind);
    sire_ID  = pests[sire_row][ID_col];
    
    offspring[offspring_count][srow_col] = sire_row;
    offspring[offspring_count][sID_col]  = sire_ID;
    
    sire_genes(pests, paras, offspring, offspring_count);
    
    paras[58]++; /* Increase the maximum ID by 1 */
}


/* =============================================================================
 * Adds asexual individuals into the population as clones of their parent
 *     pests:           The array holding the parent's information
 *     offspring:       The array that will hold the offspring's information
 *     paras:           The paras vector that holds global information
 *     ind:             The row of the mother of the individual
 *     offspring_count: The row of the offspring that is being added
 * ========================================================================== */
void add_asexual(double **pests, double **offspring, double *paras, int ind,
                 int offspring_count){
    
    int trait, cols, ID;
    int ID_col, age_col, mID_col, mrow_col, off_col, food_col, pest_col;
    int tag1_col, tag2_col, tag3_col, mate_col;
    
    cols     = (int) paras[57];  /* Columns in the pest array             */
    ID_col   = (int) paras[0];   /* Column where the ID is held           */
    age_col  = (int) paras[3];   /* Column where Age is held              */
    mID_col  = (int) paras[6];   /* Column where mum's ID is held         */
    mrow_col = (int) paras[8];   /* Column where mum's row is held        */
    off_col  = (int) paras[10];  /* Column where offspring number is held */
    food_col = (int) paras[14];  /* Column where food intake is held      */
    pest_col = (int) paras[15];  /* Column where pesticide intake is held */
    tag1_col = (int) paras[20];  /* Column where tag 1 is held            */
    tag2_col = (int) paras[21];  /* Column where tag 2 is held            */
    tag3_col = (int) paras[22];  /* Column where tag 3 is held            */
    mate_col = (int) paras[27];  /* Column where mate accessed is held    */
    
    for(trait = 0; trait < cols; trait++){
        offspring[offspring_count][trait] = pests[ind][trait];
    }
    
    offspring[offspring_count][ID_col]    = paras[58] + 1.0; 
    offspring[offspring_count][age_col]   = 0;              
    offspring[offspring_count][mID_col]   = pests[ind][0]; 
    offspring[offspring_count][mrow_col]  = ind;            
    offspring[offspring_count][off_col]   = 0;              
    offspring[offspring_count][food_col]  = 0;            
    offspring[offspring_count][pest_col]  = 0; 
    offspring[offspring_count][tag1_col]  = 0; 
    offspring[offspring_count][tag2_col]  = 0; 
    offspring[offspring_count][tag3_col]  = 0; 
    offspring[offspring_count][mate_col]  = 0;
    
    paras[58]++; /* Increase the maximum ID by 1 */
}


/* =============================================================================
 * Places the offspring in the offspring array depending on reproduction type
 *     pests:     The array holding the parent's information
 *     offspring: The array that will hold the offspring's information
 *     paras:     The paras vector that holds global information
 * ========================================================================== */
void make_offspring(double **pests, double **offspring, double *paras){
    
    int ind, N, offspring_N, offspring_col, offspring_count;
    int sex_col, sex, selfing_col, selfing;
    int *ind_offspring;
    
    N             = (int) paras[51];
    offspring_N   = (int) paras[56];
    offspring_col = (int) paras[10];
    sex_col       = (int) paras[4];
    selfing_col   = (int) paras[26];
    
    ind_offspring = malloc(N * sizeof(int *));
    for(ind = 0; ind < N; ind++){
        ind_offspring[ind] = (int) pests[ind][offspring_col];
    }
    
    offspring_count = 0;
    for(ind = 0; ind < N; ind++){
        sex = (int) pests[ind][sex_col];
        while(ind_offspring[ind] > 0){
            switch(sex){
                case 0: 
                    add_asexual(pests, offspring, paras, ind, offspring_count);
                    mutation_haploid(offspring, paras, offspring_count);
                    insert_haploid_traits(offspring, paras, offspring_count);
                    break;
                case 1: 
                    add_sexual(pests, offspring, paras, ind, offspring_count);
                    mutation_diploid(offspring, paras, offspring_count);
                    insert_diploid_traits(offspring, paras, offspring_count);
                    break;
                case 2: 
                    add_sexual(pests, offspring, paras, ind, offspring_count);
                    mutation_diploid(offspring, paras, offspring_count);
                    insert_diploid_traits(offspring, paras, offspring_count);
                    break;
                case 3:
                    break;
                default:
                    break;
            }
            offspring_count++;
            ind_offspring[ind]--;
        }
    }
    
    free(ind_offspring);
}



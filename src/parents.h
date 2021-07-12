#include "utilities.h"

void insert_diploid_traits(double **offspring, double *paras, int offspr);

void insert_haploid_traits(double **offspring, double *paras, int offspr);

void mutation_haploid(double **offspring, double *paras, int offspr);

void mutation_diploid(double **offspring, double *paras, int offspr);

void sire_genes(double **pests, double *paras, double **offspring, int offspr);

int assign_sire(double **pests, double *paras, int ind);

void add_sexual(double **pests, double **offspring, double *paras, int ind,
                int offspring_count);

void add_asexual(double **pests, double **offspring, double *paras, int ind,
                 int offspring_count);

void inbreeding_coefficient(double **offspring, double *paras, int offspr);

void make_offspring(double **pests, double **offspring, double *paras);
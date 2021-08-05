#include "utilities.h"
#include "parents.h"

void immigrant_loci_traits(double **new_pests, int row, double *paras){
  
  int sex, sex_col, ID_col, x_col, y_col, xdim, ydim;
  double mu, mu_effect, mu_sd_hap, mu_sd_dip;
  
  ID_col   = (int) paras[0];
  x_col    = (int) paras[1];
  y_col    = (int) paras[2];
  sex_col  = (int) paras[4];
  xdim     = (int) paras[103];
  ydim     = (int) paras[104];
  
  sex = (int) new_pests[row][sex_col];
  
  mu         = paras[112];
  mu_effect  = paras[114];
  mu_sd_hap  = paras[115];
  mu_sd_dip  = paras[116];
  switch(sex){
      case 0:
          paras[112] = 1.0; /* Mutation at all loci */
          paras[114] = 0.0; /* Mean loci value is 0 */
          paras[115] = 1.0; /* Mean loci stdev is 1 */
          mutation_haploid(new_pests, paras, row);
          insert_haploid_traits(new_pests, paras, row);
          break;
      case 1: 
          paras[112] = 1.0; /* Mutation at all loci */
          paras[114] = 0.0; /* Mean loci value is 0 */
          paras[116] = 1.0; /* Mean loci stdev is 1 */
          mutation_diploid(new_pests, paras, row);
          insert_diploid_traits(new_pests, paras, row);
          break;
      case 2: 
          paras[112] = 1.0; /* Mutation at all loci */
          paras[114] = 0.0; /* Mean loci value is 0 */
          paras[116] = 1.0; /* Mean loci stdev is 1 */
          new_pests[row][sex_col] = get_rand_int(2, 3);
          mutation_diploid(new_pests, paras, row);
          insert_diploid_traits(new_pests, paras, row);
          break;
      case 3:
          paras[112] = 1.0; /* Mutation at all loci */
          paras[114] = 0.0; /* Mean loci value is 0 */
          paras[116] = 1.0; /* Mean loci stdev is 1 */
          new_pests[row][sex_col] = get_rand_int(2, 3);
          mutation_diploid(new_pests, paras, row);
          insert_diploid_traits(new_pests, paras, row);
          break;
      default:
          break;
  }
  /* Return parameters back to offspring values */
  paras[112] = mu;
  paras[114] = mu_effect;
  paras[115] = mu_sd_hap;
  paras[116] = mu_sd_dip;
  
  paras[108]++;
  new_pests[row][ID_col] = (int) paras[108];
  
  new_pests[row][x_col] = get_rand_int(0, xdim - 1);
  new_pests[row][y_col] = get_rand_int(0, ydim - 1);
}


/* =============================================================================
 * Increases the age of each pest by a single time step
 *     pests:           The array holding the parent's information
 *     offspring:       The array that holds the offspring's information
 *     new_pests:       The array that will complete the time step
 *     paras:           The paras vector that holds global information
 *     imm_sample:      The first individual of a simulation to seed immigrants
 * ========================================================================== */
void fill_new_pests(double **pests, double **offspring, double **new_pests,
                    double *paras, double *imm_sample){
  
  int new_ind, old_ind, N_old, N_offspring, immigrants, col, cols;
  int mortality_col, off_ind, dead, imm_count;
  
  mortality_col = (int) paras[81];
  N_old         = (int) paras[101];
  N_offspring   = (int) paras[106];
  immigrants    = (int) paras[170];
  cols          = (int) paras[107];
  
  new_ind = 0;
  for(old_ind = 0; old_ind < N_old; old_ind++){
      dead = (int) pests[old_ind][mortality_col];
      if(dead < 1){
          for(col = 0; col < cols; col++){
              new_pests[new_ind][col] = pests[old_ind][col];
          }
          new_ind++;
      }
  }
  
  for(off_ind = 0; off_ind < N_offspring; off_ind++){
      dead = (int) offspring[off_ind][mortality_col];
      if(dead < 1){
          for(col = 0; col < cols; col++){
              new_pests[new_ind][col] = offspring[off_ind][col]; 
          }
          new_ind++;
      }
  }
  
  imm_count = 0;
  while(imm_count < immigrants){
      for(col = 0; col < cols; col++){
          new_pests[new_ind][col] = imm_sample[col]; 
      }
      immigrant_loci_traits(new_pests, new_ind, paras);
      imm_count++;
      new_ind++;
  }
}





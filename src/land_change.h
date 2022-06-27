#include "utilities.h"

double get_pesticide_val(double *paras);

void rand_pesticide(double ***land, double *paras, int *owner_choice, 
                    int max_own);

void no_pest_rot(double ***land, double *paras, int *owner_choice, int max_own);

void change_pesticide(double ***land, double *paras, int max_own);

double get_crop_val(double *paras);

void clean_pesticide(double ***land, double *paras);

void clean_crops(double ***land, double *paras);

void clean_landscape(double ***land, double *paras);

void init_crop(double ***land, double *paras, double **C_init);

void change_crop_choice(double **C_init, double **C_change, double *paras);

void land_change(double ***land, double *paras, int ts, double **C_init,
                 double **C_change);
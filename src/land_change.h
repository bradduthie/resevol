#include "utilities.h"

double get_pesticide_val(double *paras);

void init_pesticide(double ***land, double *paras, double **P_init);

void change_pesticide_choice(double **P_init, double **P_change, double *paras);

double get_crop_val(double *paras);

void clean_pesticide(double ***land, double *paras);

void clean_crops(double ***land, double *paras);

void clean_landscape(double ***land, double *paras);

void init_crop(double ***land, double *paras, double **C_init);

void change_crop_choice(double **C_init, double **C_change, double *paras);

void grow_crops(double ***land, double *grow, double *paras);

void land_change(double ***land, double *paras, int ts, double **C_init,
                 double **C_change, double **P_init, double **P_change,
                 double *grow);
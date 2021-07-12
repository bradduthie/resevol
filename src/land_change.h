#include "utilities.h"

double get_pesticide_val(double *paras);

void rand_pesticide(double ***land, double *paras, int *owner_choice, 
                    int max_own);

void no_pest_rot(double ***land, double *paras, int *owner_choice, int max_own);

void change_pesticide(double ***land, double *paras, int max_own);

double get_crop_val(double *paras);

void no_crop_rot(double ***land, double *paras, int *owner_choice, int max_own);

void rand_crop(double ***land, double *paras, int *owner_choice, int max_own);

void change_crop(double ***land, double *paras, int max_own);

void clean_landscape(double ***land, double *paras);

void land_change(double ***land, double *paras, int ts);
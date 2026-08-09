library("testthat");
library("resevol");
context("Passing individuals between runs");
set.seed(1);

# =============================================================================
# NOTE ON THE SETTINGS (why pesticide_consume = 0):
#
# pesticide_consumed is a LIFETIME accumulator: src/pesticide.c adds to it
# every time step (pests[ind][consumed_col] += consumed) and, unlike food, the
# per-layer pesticide columns are never reset (food has refresh_consumed in
# src/feeding.c; pesticide has no equivalent). Mortality then kills an
# individual once its accumulated dose exceeds pesticide_tolerated_surv
# (src/mortality.c). With pesticide_consume = 0.1 and
# pesticide_tolerated_surv = 0.1, every individual dies within ~2 steps of its
# first ingestion, so the standing population is always the newest 1-2 cohorts
# (which looks like non-overlapping generations). That is intended model
# behaviour, but it swamps the ID-machinery being tested here, so the
# ID/identity tests below set pesticide_consume = 0 to remove pesticide
# mortality entirely.
#
# food_consumed, by contrast, is a carrying-over total too (only reduced by
# metabolism), while the per-layer food_X_consumed columns reset each step.
# food_needed_surv is checked against the accumulated total, so with
# food_needed_surv = 0.25 and plentiful food nobody starves in these short
# runs.
# =============================================================================

# Reproduction enabled: lambda_value = 1 so offspring IDs advance past the
# input population's IDs. No pesticide intake and no starvation, so the
# population grows with overlapping generations and the input cohort persists
# alongside its offspring.
base_args <- list(N = 100, xdim = 40, ydim = 40, repro = "asexual",
                  neutral_loci = 10, max_age = 9,
                  min_age_move = 0, max_age_move = 9,
                  min_age_reproduce = 0, max_age_reproduce = 9,
                  min_age_feed = 0, max_age_feed = 9, food_consume = 0.25,
                  pesticide_consume = 0, rand_age = FALSE, move_distance = 1,
                  food_needed_surv = 0.25, pesticide_tolerated_surv = 0.1,
                  food_needed_repr = 0, pesticide_tolerated_repr = 100,
                  reproduction_type = "lambda", mating_distance = 1,
                  lambda_value = 1, movement_bouts = 1, selfing = TRUE,
                  feed_while_moving = FALSE, pesticide_while_moving = FALSE,
                  mortality_type = 0, age_food_threshold = 0,
                  age_pesticide_threshold = 0, farms = 4, time_steps = 2,
                  mutation_pr = 0, crossover_pr = 0, mutation_type = 0,
                  net_mu_layers = 0, net_mu_dir = 0, mutation_direction = 0,
                  crop_rotation_type = 2, crop_rotation_time = 1,
                  pesticide_rotation_type = 2, pesticide_rotation_time = 1,
                  crop_per_cell = 1, pesticide_per_cell = 1, crop_sd = 0,
                  pesticide_sd = 0, crop_min = 0, crop_max = 1000,
                  pesticide_min = 0, pesticide_max = 1000, crop_number = 2,
                  pesticide_number = 1, print_inds = FALSE, print_gens = FALSE,
                  print_last = FALSE, K_on_birth = 1000, pesticide_start = 0,
                  immigration_rate = 0, get_f_coef = FALSE, get_stats = FALSE,
                  metabolism = 0, land_edge = "torus");

# Reproduction disabled: lambda_value = 0 gives exactly 0 offspring
# (count_offspring only samples rpois(repr_param) when repr_param > 0, so
# food_needed_repr = 1000 has NO effect under lambda reproduction). Combined
# with food_needed_surv = 0 and pesticide_consume = 0, no individual dies and
# none is born, so the exact same individuals must survive from one run to the
# next. Used for the identity test.
no_repro_args <- modifyList(base_args, list(lambda_value = 0,
                                            food_needed_surv = 0,
                                            pesticide_tolerated_repr = 0));

# Like base_args but WITH pesticide intake: adults accumulate a pesticide dose
# over time while newborns must start from zero. Used to verify that parents
# do not pass their accumulated dose to offspring (both add_asexual and
# add_sexual in src/parents.c zero food_consumed, pesticide_consumed and all
# per-layer consumed columns for newborns).
pest_args <- modifyList(base_args, list(pesticide_consume = 0.1,
                                        pesticide_tolerated_surv = 100));

# mine_gmatrix is the slow part, so run it once and reuse the output.
gmt       <- matrix(data = 0, nrow = 2, ncol = 2);
diag(gmt) <- 1;
mg        <- mine_gmatrix(gmatrix = gmt, loci = 4, layers = 2, indivs = 50,
                          npsize = 50, max_gen = 2, prnt_out = FALSE);

test_that("run_farm_sim returns the final individuals as a named 3rd element", {
    skip_on_cran();
    args <- base_args;
    args$mine_output <- mg;
    sim <- do.call(run_farm_sim, args);
    expect_equal(length(sim), 3);
    expect_is(sim[[3]], "matrix");
    expect_true(nrow(sim[[3]]) >= 5);
    # column names match the in-memory layout: no "time", no "empty"
    expect_equal(length(colnames(sim[[3]])), ncol(sim[[3]]));
    expect_equal(colnames(sim[[3]])[1], "ID");
    expect_false(any(colnames(sim[[3]]) %in% c("time", "empty")));
    expect_false(any(is.na(colnames(sim[[3]]))));
    # fixed columns (100) then traits start at column 101 for traits = 2
    expect_equal(colnames(sim[[3]])[100], "ini_mean_trait_10");
    expect_equal(colnames(sim[[3]])[101], "trait_1");
})

test_that("The same individuals carry over into the next run unchanged", {
    skip_on_cran();
    args <- no_repro_args;
    args$mine_output <- mg;
    args$time_steps  <- 2;
    sim1 <- do.call(run_farm_sim, args);
    args$initial_inds <- sim1[[3]];
    sim2 <- do.call(run_farm_sim, args);
    expect_equal(length(sim2), 3);
    expect_is(sim2[[3]], "matrix");
    # no reproduction (lambda_value = 0) and no mortality, so the population
    # stays at exactly N = 100 and every individual survives into the second
    # run with its ID intact
    expect_equal(nrow(sim1[[3]]), 100);
    expect_equal(nrow(sim2[[3]]), 100);
    expect_true(all(sim1[[3]][, 1] %in% sim2[[3]][, 1]));
    # the runs actually completed: ages advance by time_steps each time
    expect_true(all(sim1[[3]][, "age"] == 2));
    expect_true(all(sim2[[3]][, "age"] == 4));
    # trait values are unchanged (no mutation, traits inherited unchanged;
    # only movement/feeding/age columns change per time step)
    m1 <- sim1[[3]][match(sim2[[3]][, 1], sim1[[3]][, 1]), 101];
    m2 <- sim2[[3]][, 101];
    expect_equal(m1, m2);
})

test_that("Offspring IDs continue past the input population (paras[108])", {
    skip_on_cran();
    args <- base_args;
    args$mine_output <- mg;
    args$time_steps  <- 3;
    sim1 <- do.call(run_farm_sim, args);
    args$initial_inds <- sim1[[3]];
    sim2 <- do.call(run_farm_sim, args);
    # new offspring start at paras[108] + 1, so the max ID must advance.
    # If paras[101] (row count) were accidentally fed the max ID instead of the
    # row count, the C code would read past the array here and this test would
    # fail or crash.
    expect_true(max(sim2[[3]][, 1]) > max(sim1[[3]][, 1]));
    # no duplicate IDs are produced across the boundary
    expect_equal(length(unique(sim2[[3]][, 1])), nrow(sim2[[3]]));
})

test_that("Chaining over several runs keeps IDs unique and growing", {
    skip_on_cran();
    args <- base_args;
    args$mine_output <- mg;
    args$time_steps  <- 3;
    sim <- do.call(run_farm_sim, args);
    max_id <- max(sim[[3]][, 1]);
    for(i in 1:2){
        args$initial_inds <- sim[[3]];
        sim <- do.call(run_farm_sim, args);
        expect_true(max(sim[[3]][, 1]) > max_id);
        expect_equal(length(unique(sim[[3]][, 1])), nrow(sim[[3]]));
        max_id <- max(sim[[3]][, 1]);
    }
})

test_that("Newborns do not inherit their parent's accumulated pesticide dose", {
    skip_on_cran();
    args <- pest_args;
    args$mine_output <- mg;
    args$time_steps  <- 3;
    sim <- do.call(run_farm_sim, args);
    d <- sim[[3]];
    # adults accumulate pesticide (0.1 per step) but pesticide_tolerated_surv
    # is high enough that they survive the short run
    expect_true(any(d[, "age"] > 0 & d[, "pesticide_consumed"] > 0));
    # every newborn (age 0, born in the final step) must start with a clean
    # slate, otherwise the parent's dose was passed on
    expect_true(all(d[d[, "age"] == 0, "pesticide_consumed"] == 0));
})

test_that("A data.frame of individuals is accepted as initial_inds", {
    skip_on_cran();
    args <- base_args;
    args$mine_output <- mg;
    args$time_steps  <- 2;
    sim1 <- do.call(run_farm_sim, args);
    args$initial_inds <- as.data.frame(sim1[[3]]);
    sim2 <- do.call(run_farm_sim, args);
    expect_is(sim2[[3]], "matrix");
})

test_that("CSV-style input with time/empty columns is rejected", {
    skip_on_cran();
    args <- base_args;
    args$mine_output <- mg;
    args$time_steps  <- 2;
    sim1 <- do.call(run_farm_sim, args);
    df_inds <- data.frame(time = 0, sim1[[3]], empty = NA);
    args$initial_inds <- df_inds;
    expect_error(do.call(run_farm_sim, args), "columns");
})

test_that("Initial_inds validation raises errors", {
    skip_on_cran();
    args <- base_args;
    args$mine_output <- mg;
    args$time_steps  <- 2;
    sim1 <- do.call(run_farm_sim, args);

    args$initial_inds <- sim1[[3]][, -1];          # wrong number of columns
    expect_error(do.call(run_farm_sim, args), "columns");

    args$initial_inds <- sim1[[3]][1:3, ];         # too few individuals
    expect_error(do.call(run_farm_sim, args), "at least 5");

    char_df <- as.data.frame(lapply(as.data.frame(sim1[[3]]), as.character));
    args$initial_inds <- char_df;                  # non-numeric
    expect_error(do.call(run_farm_sim, args), "numeric");

    bad_schema <- sim1[[3]];
    bad_schema[1, 14] <- bad_schema[1, 14] + 1;    # network_layers mismatch
    args$initial_inds <- bad_schema;
    expect_error(do.call(run_farm_sim, args));

    bad_ids <- sim1[[3]];
    bad_ids[, 1] <- bad_ids[, 1] - min(bad_ids[, 1]);  # IDs start at 0
    args$initial_inds <- bad_ids;
    expect_error(do.call(run_farm_sim, args), "positive");
})

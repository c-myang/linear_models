Linear Models
================
2022-11-10

## Model fitting

Let’s load the airbnb data.

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(borough != "Staten Island") %>% 
  select(price, stars, borough, neighborhood, room_type)
```

An good place to start is to consider price as an outcome that may
depend on rating and borough. We fit that initial model in the following
code.

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)
```

The `lm` function begins with the formula specification – outcome on the
left of the `~` and predictors separated by `+` on the right. As we’ll
see shortly, interactions between variables can be specified using `*`.
You can also specify an intercept-only model (`outcome ~ 1`), a model
with no intercept (`outcome ~ 0 + ...`), and a model using all available
predictors (`outcome ~ .`).

R will treat categorical (factor) covariates appropriately and
predictably: indicator variables are created for each non-reference
category and included in your model, and the factor level is treated as
the reference. As with `ggplot`, being careful with factors is therefore
critical!

``` r
nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type))

fit = lm(price ~ stars + borough, data = nyc_airbnb)
```

## Tidying output

The output of a `lm` is an object of class `lm` – a very specific list
that isn’t a dataframe but that can be manipulated using other
functions. Some common functions for interacting with lm fits are below,
although we omit the output.

``` r
summary(fit$coefficients) #Not great
summary(fit)
summary(fit)$coef
coef(fit)
fitted.values(fit)
```

We can tidy our model output into a clean dataframe. The `broom` package
has functions for obtaining a quick summary of the model and for
cleaning up the coefficient table.

``` r
fit %>% 
  broom::glance()
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.…¹ sigma stati…²   p.value    df  logLik    AIC    BIC devia…³
    ##       <dbl>    <dbl> <dbl>   <dbl>     <dbl> <dbl>   <dbl>  <dbl>  <dbl>   <dbl>
    ## 1    0.0342   0.0341  182.    271. 6.73e-229     4 -2.02e5 4.04e5 4.04e5  1.01e9
    ## # … with 2 more variables: df.residual <int>, nobs <int>, and abbreviated
    ## #   variable names ¹​adj.r.squared, ²​statistic, ³​deviance

``` r
fit %>% 
  broom::tidy() %>% 
  mutate(
    term = str_replace(term, "borough", "Borough: ")
  ) %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable(digits = 2)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |    19.84 |     0.1 |
| stars             |    31.99 |     0.0 |
| Borough: Brooklyn |   -49.75 |     0.0 |
| Borough: Queens   |   -77.05 |     0.0 |
| Borough: Bronx    |   -90.25 |     0.0 |

Category 1 is the reference category in regression! Let’s change the
reference category…R will automatically level categorical factors in
alphabetical order, but we want to set the reference level to the most
frequent observed category.

Note how we use pipes in `lm` - it expects a formula first and a
dataframe second. If the first argument isn’t a dataframe, we have to
specify where it goes using `.`

``` r
fit = nyc_airbnb %>% 
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type)) %>% 
  lm(price ~ stars + borough, data = .) 

fit %>% 
  broom::tidy() %>% 
  mutate(
    term = str_replace(term, "borough", "Borough: ")
  ) %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable(digits = 2)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |    19.84 |     0.1 |
| stars             |    31.99 |     0.0 |
| Borough: Brooklyn |   -49.75 |     0.0 |
| Borough: Queens   |   -77.05 |     0.0 |
| Borough: Bronx    |   -90.25 |     0.0 |

As an aside, `broom::tidy` works with lots of things, including most of
the functions for model fitting you’re likely to run into (survival,
mixed models, additive models, …).
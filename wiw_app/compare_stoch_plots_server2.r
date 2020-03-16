#' Shiny server function for the stochastic comparison
#'
#' server function to define the plots of the stochastic comparison
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_stoch_plots_server2 <- function(input, output, session, simout_a, simout_b){

  output$ComparePlot <- renderPlot({
        outa <- simout_a()
        outb <- simout_b()
        plot_compare_all_stoch(outa, outb)
  })

  output$CompareText <- renderUI({
    outa <- simout_a()
    outb <- simout_b()
    params <- outa$outputparams

    count <- list(outa$counts, outb$counts)
    count <- melt(count, id = c("age", "month", "population", "category",
                            "year", "sex", "disease", "sim")) %>%
      filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
      rename(scenario = L1) %>%
      mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"))

    totals <- count %>%
      group_by(sim, scenario) %>%
      summarize(n = sum(population)) %>%
      spread(key = scenario, value = n) %>%
      mutate(comp = A - B)

    prev <- count %>%
      group_by(sim, disease, scenario) %>%
      summarize(n = sum(population)) %>%
      spread(key = disease, value = n) %>%
      mutate(prevalence = yes/ (no + yes)) %>%
      select(sim, scenario, prevalence) %>%
      spread(key = scenario, value = prevalence) %>%
      mutate(comp = A - B)

    death <- list(outa$deaths, outb$deaths)

    hunted <- melt(death, id = c("age", "month", "population", "category",
                            "year", "sex", "sim")) %>%
      filter(age >= 2, str_sub(category, 1, 1) == "H") %>%
      rename(scenario = L1) %>%
      mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"),
             year = floor(year))

    tot.hunted <- hunted %>%
      group_by(sim, scenario) %>%
      summarize(n = sum(population)) %>%
      spread(key = scenario, value = n) %>%
      mutate(comp = A - B)

    last.hunted <- hunted %>%
      filter(round(year, 0) == max(round(year, 0))) %>%
      group_by(sim, scenario) %>%
      summarize(n = sum(population)) %>%
      spread(key = scenario, value = n) %>%
      mutate(comp = A - B)

    males.hunted <- hunted %>%
      filter(sex == "m") %>%
      group_by(sim, scenario) %>%
      summarize(n = sum(population)) %>%
      spread(key = scenario, value = n) %>%
      mutate(comp = A - B)

    males.last.hunted <- hunted %>%
      filter(round(year, 0) == max(round(year, 0))) %>%
      filter(sex == "m") %>%
      group_by(sim, scenario) %>%
      summarize(n = sum(population)) %>%
      spread(key = scenario, value = n) %>%
      mutate(comp = A - B)


    power <- power.prop.test(p1 = mean(prev$A), p2 = mean(prev$B), sig.level = 0.05, power = 0.8)

    str1 <- paste("Scenario A had a larger total population than B in",
                  length(which(totals$comp > 0 )),
                  "out of", params$sims, "simulations.", sep = " ")

    str2 <- paste("In the last year, Scenario A had a higher prevalence than B in ",
                  length(which(prev$comp > 0 )),
                  "out of", params$sims, "simulations.", sep = " ")

    str3 <- paste("Scenario A had more individuals over 2 years old hunted than B in",
                  length(which(tot.hunted$comp > 0 )),
                  "out of", params$sims, "simulations.", sep = " ")

    str4 <- paste("In the last year, Scenario A had more individuals over 2 years old hunted than B in",
                  length(which(last.hunted$comp > 0 )),
                  "out of", params$sims, "simulations.", sep = " ")

    str5 <- paste("Scenario A had more males over 2 years old hunted than B in",
                  length(which(males.hunted$comp > 0 )),
                  "out of", params$sims, "simulations.", sep = " ")

    str6 <- paste("In the last year, Scenario A had more males over 2 years old hunted than B in",
                  length(which(males.last.hunted$comp > 0 )),
                  "out of", params$sims, "simulations.", sep = " ")

    str7 <- paste("In the last year, the average prevalence in A was",
                  round(mean(prev$A),2), "while the average prevalence in B was",
                  round(mean(prev$B),2), ".", sep = " ")

    str8 <- paste("To detect this difference in prevalence with 80% power on a two-sided test you would need",
                  floor(power$n), "samples per group.", sep = " ")

    HTML(paste(str1, str2, str7, str8, str3, str4, str5, str6, sep="<br/>"))
  })
}

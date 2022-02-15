require(tidyverse)
require(lubridate)
require(patchwork)

# ///// Comparing 2019 vs 2020 counts ---------------------------------------------------------
# Main workhorse function for comparing 19vs20 death-vs-nondeath counts
compare_19vs20 <- function(df, value = value, replace_na = 0, vs=2020) {
        #' Workhorse function for comparing 19vs20 death-vs-nondeath counts
        
        if (!"censor" %in% names(df) & "death" %in% names(df)) {
                df <- df %>% rename(censor = death)
        }
        
        # may simplify
        group.by <- colnames(df)[!colnames(df) %in% c("year", "censor", "n")]
        
        if(vs == 2020){
                wide <- df %>% 
                        group_by(across(all_of(group.by))) %>%  
                        complete(year = c(2019,2020), censor = c(0,1), fill = list(n = 0)) %>% 
                        ungroup() %>% 
                        pivot_wider(names_from = c(year, censor), values_from = {{value}},
                                    names_sep = "_", names_prefix = "n")
                
                out <- wide %>% 
                        mutate(
                                n2019 = n2019_0 + n2019_1,
                                n2020 = n2020_0 + n2020_1
                        ) %>% 
                        mutate(
                                diff_0 = n2020_0 - n2019_0,
                                diff_1 = n2020_1 - n2019_1,
                                diff   = n2020   - n2019,
                                prop_0 = n2020_0 / n2019_0,
                                prop_1 = n2020_1 / n2019_1,
                                prop   = n2020   / n2019
                        )
        }else{
                wide <- df %>% 
                        filter(year %in% c("Same-period 2019", "2021")) %>%
                        mutate(year=ifelse(year=="Same-period 2019",
                                           2019,
                                           as.numeric(year))) %>%
                        group_by(across(all_of(group.by))) 
                # browser()
                wide = wide %>%
                        complete(year = c(2019,2021), censor = c(0,1), fill = list(n = 0)) %>% 
                        ungroup() %>% 
                        pivot_wider(names_from = c(year, censor), values_from = {{value}},
                                    names_sep = "_", names_prefix = "n")
                # browser()
                out <- wide %>% 
                        mutate(
                                n2019 = n2019_0 + n2019_1,
                                n2021 = n2021_0 + n2021_1
                        ) %>% 
                        mutate(
                                diff_0 = n2021_0 - n2019_0,
                                diff_1 = n2021_1 - n2019_1,
                                diff   = n2021   - n2019,
                                prop_0 = n2021_0 / n2019_0,
                                prop_1 = n2021_1 / n2019_1,
                                prop   = n2021   / n2019
                        )
        }
        
        return(out)
}

impute21 = function(daily){
        # mutate year and assign 2019 same-period 
        if("eventdate" %in% names(daily)){
                daily = daily %>% 
                        mutate(year=year(eventdate),
                               yday=yday(eventdate))
        }
        daily = daily %>% 
                mutate(year = as.character(year))
        # 2021 dates
        dates.2021 = daily %>% 
                filter(year == 2021) %>%
                select(yday) %>%
                unique() %>%
                pull(yday)
        # make 2019 same-period dataframe
        daily.sameperiod = daily %>%
                filter(yday %in% dates.2021, year==2019) %>%
                mutate(year = "Same-period 2019")
        # bind_rows to original daily dataframe
        daily = bind_rows(daily, daily.sameperiod)
        return(daily)
}

compare_192021 = function(daily){
        out.changes = daily %>%
                pivot_wider(names_from = "year", names_prefix = "n", values_from = "n") %>%
                mutate(diff.2020 = n2020-n2019,
                       diff.2021 = n2021 - `nSame-period 2019`,
                       percent.diff.2020 = signif(diff.2020/n2019*100,3),
                       percent.diff.2021 = signif(diff.2021/`nSame-period 2019`*100,3)) %>% 
                select(-n2020, -n2021, -diff.2020, -diff.2021)
        return(out.changes)
}

# ///// Plot_eda ---------------------------------------------------------
# Basically a wrapper for 
# (i) plot_rollmean_death_preview, which plots the rollmean of deaths and attendance;
# (ii) plot_compare_death, which basic barchart showing deaths and attendance; and 
# (iii) summarise_death_change_stats, which computes basic counting stats of deaths and attendance.
# # There three functions can be called directly

plot_eda <- function(df, wave.df = NULL, pal.death = NULL, rollmean = T, title = NULL) {
        #' Plot EDA of a subsetted dataframe
        
        if (is.null(wave.df)) wave.df <- import_waves()
        if (is.null(pal.death)) pal.death <- c(
                "1" = "#F8766D", # death = pink
                "0" =  "#7CAE00", # survived = green
                "-1" = "#00BFC4" # overall = cyan
        )
        
        lineplot <- plot_rollmean_death_preview(df, wave.df = wave.df, pal.death = pal.death)
        
        figA <- plot_compare_death(df, pal.death = pal.death, zoom = F) &
                theme(legend.position = "none")
        
        patchwork <- (
                figA
                + lineplot
                + plot_annotation(title = title, subtitle = summarise_death_change_stats(df))
                + plot_layout(widths = c(1,1,5))
                
        )

        return(patchwork)
}

## helper functions for plot_eda ----------------------------------------------------------
summarise_death_change_stats <- function(df) {
        table <- df %>% 
                count(year, death) %>% 
                compare_19vs20(n)
        
        rate19 <- round(table$n2019_1 / table$n2019 * 100, 2)
        rate20 <- round(table$n2020_1 / table$n2020 * 100, 2)
        deathpc <- round(table$prop_1 * 100 - 100, 2)
        
        summary <- paste0("2019: ", table$n2019_1, " deaths / ", table$n2019, " attn (", rate19, "%)", "\n",
                          "2020: ", table$n2020_1, " deaths / ", table$n2020, " attn (", rate20, "%)", "\n",
                          "            ", table$diff_1, " (", deathpc, "%) change in deaths")
        
        return(summary)
}

plot_rollmean_death_preview <- function(df, wave.df = NULL, pal.death = NULL) {
        #' Plot a rollmean line plot of a subsetted dataframe of ED records
        
        if (is.null(wave.df)) wave.df <- import_waves()
        if (is.null(pal.death)) pal.death <- c(
                "1" = "#F8766D", # death = pink
                "0" =  "#7CAE00", # survived = green
                "-1" = "#00BFC4" # overall = cyan
        )
        
        axis.text.x.bottom <- element_text(size = 8, hjust = 1, vjust = 1, angle = 45)
        
        #### Dataframe setup
        cts <- df %>% 
                select(year, eventdate, death) %>% 
                count(year, eventdate, death) %>% 
                bind_rows(group_by(., year, eventdate) %>% summarise(n = sum(n), .groups = "drop") %>% mutate(death = -1)) %>% 
                mutate(
                        year = factor(year),
                        death = factor(death)
                ) %>% 
                arrange(year, eventdate, death)
        
        ### PLOTS
        df <- cts %>% 
                filter(death != -1) %>% 
                mutate(death = ifelse(death == 1, "deaths", "non-deaths")) %>% 
                mutate(eventdate = as_date(ifelse(year == 2019, eventdate + years(1), eventdate))) %>% 
                group_by(death, year) %>% 
                mutate(rollmean = zoo::rollmean(n, 7, na.pad = T, align = "right"))
        
        p <- ( df %>% 
                       ggplot(aes(x = eventdate, group = paste(year, death), col = year))
               + geom_point(aes(y = n), alpha = 0.2)
               + geom_line(aes(y = rollmean))
               + facet_wrap(~death, ncol = 1, scales = "free_y") 
               + scale_x_date(
                       date_labels = "%d-%b-%Y",
                       date_breaks = "1 month",
                       # date_minor_breaks = "1 week",
                       expand = c(0,0),
                       breaks = ymd("2020-10-15")
               )
               + geom_rect(data = filter(wave.df, year == 2020), 
                           aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
                           fill="red", alpha=0.1, inherit.aes = FALSE)
               + ggpubr::theme_pubclean()
               + theme(legend.position = "none",
                       axis.text.x = axis.text.x.bottom,
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank())
        )
        
        return(p)
}


plot_compare_death <- function(df, pal.death = NULL, zoom = FALSE) {
        #' Plot a line plot of deaths and ED Attendance of a subsetted ED records dataframe
        
        if (is.null(pal.death)) pal.death <- c(
                "1" = "#F8766D", # death = pink
                "0" =  "#7CAE00", # survived = green
                "-1" = "#00BFC4" # overall = cyan
        )
        
        ### Dataframe setup
        cts.overall <- df %>% 
                count(year, death) %>% 
                mutate(
                        year = factor(year),
                        death = factor(death)
                )
        y_limit <- cts.overall %>% filter(death == 1) %>% pull(n) %>% max()
        y_limit <- y_limit * 2
        
        ### Plotting
        fig.overall <- (cts.overall %>% 
                                ggplot(aes(x = year, y = n, fill = death))
                        + geom_col()
                        + xlab("")
                        + ylab("ED Attendance")
        )
        
        if (zoom == T) {
                #### facet zoom
                figA.zoom <- ( fig.overall
                               + facet_zoom(ylim = c(0, y_limit),
                                            y = death == 1,
                                            zoom.data = c(F,NA,F,NA),
                                            zoom.size = 1)
                               + scale_fill_manual(name = "28-day mortality",
                                                   breaks = names(pal.death),
                                                   labels = c("Died", "Survived", "ED Attendance"),
                                                   values = pal.death)
                )
                
                return(figA.zoom)
        }
        
        
        ### output if not use facet_zoom
        death <- ( cts.overall %>% 
                           filter(death == 1) %>% 
                           ggplot(aes(x = year, y = n, fill = death))
                   + geom_col()
                   + xlab("")
                   + ylab("")
        )
        
        patchwork <- (
                fig.overall
                + death
                # & plot_layout(guides = "collect")
                & scale_fill_manual(name = "28-day Mortality",
                                    breaks = names(pal.death),
                                    labels = c("Died", "Survived", "ED Attendance"),
                                    values = pal.death)
                & scale_y_continuous(
                        expand = expansion(mult = c(0, 0.2)),
                        labels = scales::label_number(suffix = " K", big.mark = "", scale = 1e-3)
                )
                & theme(panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black")
                )
        )
        return(patchwork)
}


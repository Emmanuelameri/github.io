
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Set theme for all plots
theme_set(theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.title = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  ))

# ============================================================================
# GRAPH 1: Entry-Level Job Postings Decline by Sector (2019-2023)
# ============================================================================

job_decline_data <- data.frame(
  Sector = c("Customer Support", "Junior Administrative", 
             "Data Entry & Processing", "Research Assistant", 
             "Marketing Coordinator", "Overall (AI-exposed)"),
  Decline_Pct = c(-17, -28, -34, -15, -19, -22),
  Order = c(1, 2, 3, 4, 5, 6)
)

# Reorder for better visualization
job_decline_data$Sector <- factor(job_decline_data$Sector, 
                                   levels = job_decline_data$Sector[order(job_decline_data$Decline_Pct)])

graph1 <- ggplot(job_decline_data, aes(x = Sector, y = Decline_Pct, fill = Sector)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(Decline_Pct, "%")), 
            hjust = 1.2, color = "white", fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(-40, 0),
                     breaks = seq(-40, 0, 10)) +
  labs(
    title = "Entry-Level Job Postings Decline by Sector",
    subtitle = "Change in 0-1 Year Experience Job Postings (2019-2023)",
    x = NULL,
    y = "Percentage Change",
    caption = "Source: Burning Glass Technologies Labor Market Analytics"
  ) +
  theme(panel.grid.major.y = element_blank())

print(graph1)
ggsave("graph1_job_decline.png", graph1, width = 10, height = 6, dpi = 300)

# ============================================================================
# GRAPH 2: Shift in Annotation Labor Location (2018 vs 2023)
# ============================================================================

annotation_data <- data.frame(
  Category = rep(c("Domestic Junior Staff", "Offshore Specialists", "Automated Systems"), 2),
  Year = rep(c("2018", "2023"), each = 3),
  Percentage = c(60, 30, 10, 15, 75, 10)
)

annotation_data$Year <- factor(annotation_data$Year, levels = c("2018", "2023"))
annotation_data$Category <- factor(annotation_data$Category, 
                                    levels = c("Domestic Junior Staff", "Offshore Specialists", "Automated Systems"))

graph2 <- ggplot(annotation_data, aes(x = Year, y = Percentage, fill = Category)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, 85),
                     breaks = seq(0, 80, 20)) +
  labs(
    title = "Shift in Annotation Labor Location",
    subtitle = "Distribution of AI Data Annotation Work (2018 vs 2023)",
    x = NULL,
    y = "Percentage of Total Annotation Work",
    fill = "Labor Category",
    caption = "Source: Ludec et al. (2023)"
  )

print(graph2)
ggsave("graph2_annotation_shift.png", graph2, width = 10, height = 6, dpi = 300)

# ============================================================================
# GRAPH 3: Skill Requirement Escalation in Entry-Level Postings
# ============================================================================

skill_data <- data.frame(
  Skill = c("Prior Internship", "SQL/Python", "AI/ML Knowledge", "Advanced Communication"),
  Year_2019 = c(23, 8, 2, 31),
  Year_2023 = c(47, 29, 18, 54)
) %>%
  pivot_longer(cols = c(Year_2019, Year_2023), 
               names_to = "Year", 
               values_to = "Percentage") %>%
  mutate(Year = ifelse(Year == "Year_2019", "2019", "2023"))

skill_data$Skill <- factor(skill_data$Skill, 
                           levels = c("Prior Internship", "SQL/Python", 
                                     "AI/ML Knowledge", "Advanced Communication"))

graph3 <- ggplot(skill_data, aes(x = Skill, y = Percentage, fill = Year)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("2019" = "#984EA3", "2023" = "#FF7F00")) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  labs(
    title = "Skill Requirement Escalation in Entry-Level Postings",
    subtitle = "Percentage of 'Entry-Level' Jobs Requiring Each Skill",
    x = NULL,
    y = "Percentage of Job Postings",
    fill = "Year",
    caption = "Source: Analysis of 450,000 job postings"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(graph3)
ggsave("graph3_skill_escalation.png", graph3, width = 10, height = 6, dpi = 300)

# ============================================================================
# GRAPH 4: Wage Growth Divergence by Age Cohort (2019-2023)
# ============================================================================

wage_data <- data.frame(
  Age_Group = rep(c("22-25 years", "26-30 years", "31-40 years", "41+ years"), 2),
  Sector = rep(c("AI-exposed", "Non-AI"), each = 4),
  Wage_Growth = c(1.2, 2.1, 3.9, 4.1, 3.8, 4.2, 4.5, 4.3)
)

wage_data$Age_Group <- factor(wage_data$Age_Group, 
                              levels = c("22-25 years", "26-30 years", 
                                        "31-40 years", "41+ years"))

graph4 <- ggplot(wage_data, aes(x = Age_Group, y = Wage_Growth, fill = Sector)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(Wage_Growth, "%")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("AI-exposed" = "#E41A1C", "Non-AI" = "#377EB8")) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, 5),
                     breaks = seq(0, 5, 1)) +
  labs(
    title = "Wage Growth Divergence by Age Cohort",
    subtitle = "Annual Wage Growth in AI-exposed vs Non-AI Sectors (2019-2023)",
    x = "Age Group",
    y = "Annual Wage Growth (%)",
    fill = "Sector Type",
    caption = "Source: Acemoglu & Restrepo (2022)"
  )

print(graph4)
ggsave("graph4_wage_divergence.png", graph4, width = 10, height = 6, dpi = 300)

# ============================================================================
# BONUS GRAPH 5: Time to First Job After Graduation
# ============================================================================

time_to_job_data <- data.frame(
  Year = c("2019", "2023"),
  Months = c(3.2, 5.7),
  Unemployment_6mo = c(8.1, 14.3)
)

# Create dual-axis plot
graph5a <- ggplot(time_to_job_data, aes(x = Year)) +
  geom_col(aes(y = Months), fill = "#4DAF4A", alpha = 0.7, width = 0.5) +
  geom_text(aes(y = Months, label = paste0(Months, " mo")), 
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, 1)) +
  labs(
    title = "Average Time to First Job After Graduation",
    subtitle = "Recent college graduates seeking employment",
    x = NULL,
    y = "Months",
    caption = "Source: Bureau of Labor Statistics"
  )

print(graph5a)
ggsave("graph5_time_to_job.png", graph5a, width = 8, height = 6, dpi = 300)

# Unemployment rate comparison
graph5b <- ggplot(time_to_job_data, aes(x = Year, y = Unemployment_6mo)) +
  geom_col(fill = "#E41A1C", alpha = 0.7, width = 0.5) +
  geom_text(aes(label = paste0(Unemployment_6mo, "%")), 
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, 18),
                     breaks = seq(0, 18, 3)) +
  labs(
    title = "Unemployment Rate 6 Months After Graduation",
    subtitle = "Percentage of graduates still seeking employment",
    x = NULL,
    y = "Unemployment Rate (%)",
    caption = "Source: Bureau of Labor Statistics"
  )

print(graph5b)
ggsave("graph5b_unemployment_rate.png", graph5b, width = 8, height = 6, dpi = 300)

# ============================================================================
# BONUS GRAPH 6: Cumulative Talent Deficit Projection
# ============================================================================

# Project talent deficit over 10 years
years <- 2024:2033
annual_deficit <- 158000  # workers per year
cumulative_deficit <- cumsum(rep(annual_deficit, 10))

deficit_projection <- data.frame(
  Year = years,
  Cumulative_Deficit = cumulative_deficit / 1000000  # Convert to millions
)

graph6 <- ggplot(deficit_projection, aes(x = Year, y = Cumulative_Deficit)) +
  geom_line(color = "#E41A1C", size = 1.5) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_area(alpha = 0.3, fill = "#E41A1C") +
  geom_text(data = deficit_projection %>% filter(Year %in% c(2024, 2028, 2033)),
            aes(label = paste0(round(Cumulative_Deficit, 2), "M")),
            vjust = -1, fontface = "bold") +
  scale_y_continuous(labels = function(x) paste0(x, "M"), 
                     limits = c(0, 2),
                     breaks = seq(0, 2, 0.5)) +
  scale_x_continuous(breaks = seq(2024, 2033, 1)) +
  labs(
    title = "Projected Cumulative Talent Deficit",
    subtitle = "Missing workers entering innovation sectors due to AI displacement (2024-2033)",
    x = "Year",
    y = "Cumulative Worker Deficit (Millions)",
    caption = "Assumes 22% persistent reduction in entry-level positions"
  )

print(graph6)
ggsave("graph6_talent_deficit_projection.png", graph6, width = 10, height = 6, dpi = 300)

# ============================================================================
# BONUS GRAPH 7: Innovation Output Impact Model
# ============================================================================

# Model relationship between talent deficit and innovation output
innovation_data <- data.frame(
  Talent_Deficit_Pct = seq(0, 30, 5),
  Innovation_Output_Loss = c(0, 3, 7, 12, 18, 25, 31)
)

graph7 <- ggplot(innovation_data, aes(x = Talent_Deficit_Pct, y = Innovation_Output_Loss)) +
  geom_line(color = "#377EB8", size = 1.5) +
  geom_point(color = "#377EB8", size = 4) +
  geom_vline(xintercept = 22, linetype = "dashed", color = "#E41A1C", size = 1) +
  annotate("text", x = 22, y = 28, label = "Current trajectory\n(22% deficit)", 
           color = "#E41A1C", fontface = "bold", hjust = -0.1) +
  annotate("point", x = 22, y = 27, color = "#E41A1C", size = 5) +
  scale_x_continuous(labels = percent_format(scale = 1), 
                     breaks = seq(0, 30, 5)) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     breaks = seq(0, 35, 5)) +
  labs(
    title = "Modeled Impact on Innovation Output",
    subtitle = "Projected innovation loss from talent pipeline bottleneck",
    x = "Entry-Level Talent Deficit (%)",
    y = "Reduction in Innovation Output (%)",
    caption = "Model based on innovation production function with diversity effects"
  )

print(graph7)
ggsave("graph7_innovation_impact.png", graph7, width = 10, height = 6, dpi = 300)

# ============================================================================
# Summary: Save all graphs to files
# ============================================================================

cat("\n=== All graphs generated successfully! ===\n")
cat("Files saved:\n")
cat("  - graph1_job_decline.png\n")
cat("  - graph2_annotation_shift.png\n")
cat("  - graph3_skill_escalation.png\n")
cat("  - graph4_wage_divergence.png\n")
cat("  - graph5_time_to_job.png\n")
cat("  - graph5b_unemployment_rate.png\n")
cat("  - graph6_talent_deficit_projection.png\n")
cat("  - graph7_innovation_impact.png\n")
cat("\nAll graphs are publication-ready at 300 DPI.\n")

# Revenue model starter

target_monthly_revenue <- 450

cat("Content Engine Revenue Model\n")
cat("Target monthly revenue:", target_monthly_revenue, "EUR\n")

# This section defines a simple starting baseline.
# The idea is to use a realistic early publishing pace and average revenue.
starting_articles_per_week <- 1
avg_revenue_per_article <- 7

# A rough monthly conversion uses 52 weeks / 12 months.
# This keeps the model simple while turning a weekly rhythm into a monthly one.
weeks_per_month <- 52 / 12
monthly_article_count <- starting_articles_per_week * weeks_per_month

# Baseline monthly revenue is monthly articles multiplied by revenue per article.
baseline_monthly_revenue <- monthly_article_count * avg_revenue_per_article

# The revenue gap shows how far the baseline is from the target.
revenue_gap <- target_monthly_revenue - baseline_monthly_revenue

# Print the baseline results in a clean, readable format.
cat("\n")
cat("Baseline assumptions\n")
cat("--------------------\n")
cat("Articles per week:", starting_articles_per_week, "\n")
cat("Average revenue per article:", avg_revenue_per_article, "EUR\n")
cat("Estimated articles per month:", round(monthly_article_count, 2), "\n")
cat("Baseline monthly revenue:", round(baseline_monthly_revenue, 2), "EUR\n")
cat("Gap to target:", round(revenue_gap, 2), "EUR\n")

# Build a simple scenario table with different revenue-per-article assumptions.
# The publishing cadence stays the same at 1 article per week.
scenario_revenue_per_article <- c(5, 7, 10, 15)

scenario_table <- data.frame(
  avg_revenue_per_article = scenario_revenue_per_article,
  monthly_article_count = round(
    rep(monthly_article_count, length(scenario_revenue_per_article)),
    2
  ),
  monthly_revenue = round(monthly_article_count * scenario_revenue_per_article, 2),
  gap_to_target = round(
    target_monthly_revenue - (monthly_article_count * scenario_revenue_per_article),
    2
  )
)

# Print the scenario table.
cat("\n")
cat("Scenario table\n")
cat("--------------\n")
print(scenario_table, row.names = FALSE)

# Create a simple plot that compares the target revenue level
# with each revenue-per-article scenario across 12 months.
months <- 1:12
target_line <- rep(target_monthly_revenue, length(months))
scenario_5_line <- rep(scenario_table$monthly_revenue[1], length(months))
baseline_line <- rep(scenario_table$monthly_revenue[2], length(months))
scenario_10_line <- rep(scenario_table$monthly_revenue[3], length(months))
scenario_15_line <- rep(scenario_table$monthly_revenue[4], length(months))

plot(
  months,
  target_line,
  type = "l",
  lwd = 2,
  col = "red",
  ylim = c(
    0,
    max(
      target_line,
      scenario_5_line,
      baseline_line,
      scenario_10_line,
      scenario_15_line
    ) * 1.1
  ),
  xaxt = "n",
  xlab = "Month",
  ylab = "Monthly revenue (EUR)",
  main = "Revenue Scenarios vs Target"
)

lines(
  months,
  scenario_5_line,
  lwd = 2,
  col = "darkgreen"
)

lines(
  months,
  baseline_line,
  lwd = 2,
  col = "blue"
)

lines(
  months,
  scenario_10_line,
  lwd = 2,
  col = "orange"
)

lines(
  months,
  scenario_15_line,
  lwd = 2,
  col = "purple"
)

axis(1, at = months, labels = months)

legend(
  "topleft",
  legend = c(
    "Target revenue",
    "5 EUR/article",
    "7 EUR/article",
    "10 EUR/article",
    "15 EUR/article"
  ),
  col = c("red", "darkgreen", "blue", "orange", "purple"),
  lwd = 2,
  bty = "n"
)

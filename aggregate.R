library(ggplot2)
library(scales)
theme_set(theme_classic())

component_aggregate = read.csv("data/component-aggregate.csv", header = T)
row.names(component_aggregate) <- component_aggregate$component
component = component_aggregate[c(1)]
nclick = component_aggregate[c(2)]
published = component_aggregate[c(3)]

df = data.frame(component = component, aggregate = published)
best5 = head(df[order(- df$publish),], 5)
ggplot(best5, aes(x = "", y = publish, fill = component)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = component), position = position_stack(vjust = 0.5)) +
    labs(fill = NULL,
    x = NULL,
    y = NULL,
    title = "Published Components Best 5",
    theme(axis.text.x = element_blank()))
ggsave("result/published_components.jpeg")

df = data.frame(component = component_aggregate[c(1)], nclick)
best5 = head(df[order(- df$nclick),], 5)
ggplot(best5, aes(x = "", y = nclick, fill = component)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = component), position = position_stack(vjust = 0.5)) +
    labs(fill = NULL,
    x = NULL,
    y = NULL,
    title = "Clicked Components Best 5",
    theme(axis.text.x = element_blank()))
ggsave("result/clicked_components.jpeg")

df = data.frame(component = component, published = published / nclick)
ggplot(df, aes(x = component, y = publish)) +
    geom_point(size = 3) +
    geom_segment(aes(x = component,
    xend = component,
    y = 0,
    yend = publish)) +
    labs(title = "Published Ratio") +
    theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
ggsave("result/published_ratio.jpeg")

image_freq = read.csv("data/image-freq.csv", header = T)
df = data.frame(image_freq)
ggplot(df, aes(fill = used_count, y = num_of_users, x = method)) +
    geom_bar(aes(fill = used_count), width = 0.4, stat = "identity") +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.6)) +
    labs(title = "Image Usage",
    x = NULL,
    y = "Number of Users",
    caption = "SmartEditor")
ggsave("result/image-freq.jpeg")

active_users = read.csv("data/active_users.csv", header = T)

active_users$month <- factor(active_users$month, levels = active_users$month[order(active_users$month_num)])
df1 <- data.frame(month = active_users$month, user = active_users$active_user)
df2 <- data.frame(month = active_users$month, user = active_users$user)

ggplot(df1, aes(month, user, group = 1)) +
    geom_line(aes(color = "Active Users")) +
    geom_line(data = df2, aes(color = "Whole Users")) +
    scale_y_log10(limits = c(1100, 5750)) +
    labs(title = "Growing Tendency", color = NULL, x = NULL, y = "Number of Users")
ggsave("result/active_users.jpeg")
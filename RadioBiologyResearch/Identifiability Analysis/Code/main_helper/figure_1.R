###Creating Figure 1 showing Concavity
d1 = seq(0, 1e-06, length.out = 50)
d2 = seq(1e-05, 1e-04, length.out = 30)
d3 = seq(1e-04, 1e-03, length.out = 20)
d4 = seq(1e-03, 1e-02, length.out = 20)
d5 = seq(1e-02, 4e-01, length.out = 30)

d= c(d1, d2, d3, d4, d5, 4e-05)
d = sort(d)
normal = ggplot() + geom_errorbar(data = big_df[2:9, ], aes(x = d, ymin = 100*errorbar_lower, ymax = 100*errorbar_upper), col = "black", width = 0)  + geom_point(aes(x = modified_df$d[1:8], y = 100*modified_df$CA[1:8]), size = 3) + geom_line(aes(x = d,  y = 100*(IDER(d = d, L = 75, Z.beta = 595))), col = "black") + labs(x = "Dose (Gy)", y = "Prevalence (%)")  + annotate("text",x = 0.025, y = 6, label = "atop(bold('A'))", size = 8, parse = TRUE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=19), axis.title.y = element_text(vjust = 7), plot.margin = margin(10, 10, 10, 20)) 

d1 = seq(0, 4e-05, length.out = 500)
zoomed = c("Zoomed in Along", "Horizontal Axis 5,000x")
zoomed_very_in = ggplot() + geom_line(aes(x = d1,  y = 100*(IDER(d = d1, L = 75, Z.beta = 595))), col = "black") + labs(x = "Dose (Gy)", y = "Prevalence (%)", title = "")  + ylim(0, 1.1)  + annotate("text",x = 0.5e-05, y = 0.75, label = "atop(bold('B'))", size = 8, parse = TRUE) + theme(axis.text=element_text(size=15), axis.title=element_text(size=19), axis.title.y = element_text(vjust = 7), plot.margin = margin(10, 10, 10, 20)) + annotate("text",x = 2.5e-05, y = c(0.25, 0.15), label = zoomed, size = 6) + scale_x_continuous(breaks = c(0, 2e-5, 4e-5), limits = c(0, 4e-5))

a = ggplotGrob(normal)
b = ggplotGrob(zoomed_very_in)

b$heights = a$heights
b$widths = a$widths

pdf("concavity_IDERs.pdf", width = 12, height = 6)
grid.arrange(a,b, nrow = 1)
dev.off()
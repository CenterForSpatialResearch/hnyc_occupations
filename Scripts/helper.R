sub1 = combined %>% filter(year == 1850) %>% .[c(1:10), ]
sub2 = combined %>% filter(year == 1880) %>% .[c(1:10), ]
sub3 = combined %>% filter(year == 1910) %>% .[c(1:10), ]

sub = rbind(sub1, sub2)
sub = rbind(sub, sub3)

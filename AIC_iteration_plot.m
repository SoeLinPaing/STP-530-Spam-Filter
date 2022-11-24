AIC = [4942.947 3905.680 3380.965 2975.237 2720.094 2488.170 2318.126 2173.974 ... 
    2081.437 1994.325 1932.384 1887.208 1852.721 1816.932 1787.860 1760.147 ...
    1738.575 1720.902 1702.630 1685.197 1669.860 1654.109 1643.330 1635.289 ...
    1627.798 1621.251 1615.482 1610.545 1603.712 1598.897 1590.060 1585.379 ...
    1580.731 1577.002 1573.189 1570.771 1567.891 1565.583 1563.138 1561.921 ...
    1561.238 1560.589 1560.362];

figure
plot(AIC, 'linewidth', 2)
ylabel('AIC')
xlabel('iteration')
title('Model reduction using AIC')



figure
plot(inter_AIC, 'linewidth', 2)
ylabel('AIC')
xlabel('iteration')
title('Interaction model search using AIC')
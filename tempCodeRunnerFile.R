fpgrowth.itemsets <- fpgrowth(retail.transformed,
                              parameter=list(
                                supp=0.02,
                                target='frequent itemsets'
                              ))
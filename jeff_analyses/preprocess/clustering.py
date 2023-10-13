import snafu # https://github.com/AusterweilLab/snafu-py

fluencydata = snafu.load_fluency_data("../data/results_noperseveration.csv",
                                       scheme="../misc/snafu_scheme.csv")

avg_num_cluster_switches = snafu.clusterSwitch(fluencydata.labeledlists, "../misc/snafu_scheme.csv", clustertype="fluid")
avg_cluster_sizes = snafu.clusterSize(fluencydata.labeledlists, "../misc/snafu_scheme.csv", clustertype="fluid")

sub_list, listnum_list = zip(*fluencydata.listnums)

# create a list of tuples, with each tuple being a row you can write to CSV
to_write = list(zip(
            sub_list,
            listnum_list,
            avg_num_cluster_switches,
            avg_cluster_sizes
            ))

# write data to a file!
with open('../data/clustering.csv','w') as fo:
    fo.write("id,listnum,num_cluster_switches,avg_cluster_size\n")
    for line in to_write:
        fo.write(",".join([str(i) for i in line]) + "\n")



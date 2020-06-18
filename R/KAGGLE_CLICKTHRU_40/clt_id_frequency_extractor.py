from collections import defaultdict

if __name__ == "__main__":
    WRT = 'KAGGLE/CLICKTHRU/train'

    site = defaultdict(int)
    site_cat = defaultdict(int)
    site_no = defaultdict(int)
    site_cat_no = defaultdict(int)

    app = defaultdict(int)
    app_cat = defaultdict(int)
    app_no = defaultdict(int)
    app_cat_no = defaultdict(int)

    N = 0
    C = 0
    for line in open(WRT):
        N = N + 1
        line = line.strip()
        items = line.split(',') 
        ad_id   = items[1-1]
        click   = items[2-1]

        #reversed
        app_id  = items[6-1]
        app_cat_id = items[8-1]
        site_id = items[9-1]
        site_cat_id = items[11-1]

        if "1" in click:
            C = C + 1
            site[site_id] = site[site_id] + 1
            site_cat[site_cat_id] = site_cat[site_cat_id] + 1
            app[app_id] = app[app_id] + 1
            app_cat[app_cat_id] = app_cat[app_cat_id] + 1
        else:
            site_no[site_id] = site_no[site_id] + 1
            site_cat_no[site_cat_id] = site_cat_no[site_cat_id] + 1
            app_no[app_id] = app_no[app_id] + 1
            app_cat_no[app_cat_id] = app_cat_no[app_cat_id] + 1

    print N, C

    site_keys = sorted(site.keys(), key=lambda(x) : site[x])
    site_cat_keys = sorted(site_cat.keys(), key=lambda(x) : site[x])
    with open('site_id.csv', 'w') as fp:
        fp.write("site_id,site_catcf,site_catcp\n")
        for key in site_keys:
            out = "%s,%s,%s\n" % (key, site[key], float(site[key])/(float(site_no[key]) + 1 + float(site[key])) * 1)
            fp.write(out)

    with open('site_id_cat.csv', 'w') as fp:
        fp.write("site_id,site_catcf,site_catcp\n")
        for key in site_cat_keys:
            out = "%s,%s,%s\n" % (key, site_cat[key], float(site_cat[key])/(float(site_cat_no[key]) + 1 + float(site_cat[key])) * 1)
            fp.write(out)



    site_keys = sorted(app.keys(), key=lambda(x) : app[x])
    site_cat_keys = sorted(app_cat.keys(), key=lambda(x) : app[x])

    site = app
    site_no = app_no
    with open('app_id.csv', 'w') as fp:
        fp.write("site_id,site_catcf,site_catcp\n")
        for key in site_keys:
            out = "%s,%s,%s\n" % (key, site[key], float(site[key])/(float(site_no[key]) + 1 + float(site[key])) * 1)
            fp.write(out)

    site_cat = app_cat
    site_cat_no = app_cat_no
    with open('app_id_cat.csv', 'w') as fp:
        fp.write("site_id,site_catcf,site_catcp\n")
        for key in site_cat_keys:
            out = "%s,%s,%s\n" % (key, site_cat[key], float(site_cat[key])/(float(site_cat_no[key]) + 1 + float(site_cat[key])) * 1)
            fp.write(out)


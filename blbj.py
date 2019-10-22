import pandas as pd
import pymysql
import sqlalchemy
pd.set_option('mode.use_inf_as_na', True)

#engine = sqlalchemy.create_engine('mysql+pymysql://blbjpc:Blbjpc123456@rm-bp1o1359kmg9ay34yso.mysql.rds.aliyuncs.com:3306/伯乐百家爬虫?charset=utf8')
engine = sqlalchemy.create_engine('mysql+pymysql://root:Blbj123456@rm-bp16nmlmn159wru4reo.mysql.rds.aliyuncs.com:3306/blbj_crawler?charset=utf8')

sale = pd.read_sql("select * from 房管网_每日数据详细",con=engine)
sale.loc[~sale['类别'].isin(['住宅','成套住宅','成 住宅','成住宅','成  住宅','别墅','宿舍','商业','产权式酒店','商铺','酒店','商店','公寓式酒店','餐饮','商场','商贸','商务设施','商业附房','宾馆','金融','饭店','农贸市场']),'类别_2']="其他"
sale.loc[sale['类别'].isin(['住宅','成套住宅','成 住宅','成住宅','成  住宅','别墅']),'类别_2']="住宅"
sale.loc[sale['类别'].isin(['宿舍','商业','产权式酒店','商铺','酒店','商店','公寓式酒店','餐饮','商场','商贸','商务设施','商业附房','宾馆','金融','饭店','农贸市场']),'类别_2']="商业"
junjia = pd.read_sql("select * from 房管网_销售项目2层_供应房源数据",con=engine).drop_duplicates(["预售许可证号","项目名称"],keep="last")
tuiguangming = pd.read_sql("select * from 推广名",con=engine)
#jiage商业住宅etl
jiage = pd.read_sql("select * from 价格",con=engine)
jiage.loc[jiage['备注'].isin(['厂房','办公','办公用房','车位','销售展示厅','仓库','办公室','工业','车库','创客空间','农具间','物业01','物业02','物业经营用房','发电机房','其他','消防控制室','开闭所','生产车间','弱电机房','消控室','物业用房','不可售','不在项目内','洗手间','物业','产品展示中心（工业）']),'类别']="其他"
jiage.loc[jiage['备注'].isin(['商业','商住房','酒店套房','店面','商铺','产权式酒店','农贸市场','度假屋','[商业]餐饮','[酒店]客房','[酒店]样板房','商务酒店','宿舍']),'类别']="商业"
jiage.loc[~jiage['备注'].isin(['商业','商住房','酒店套房','店面','商铺','产权式酒店','厂房','办公','车位','销售展示厅','仓库','办公室','工业','车库','创客空间','农具间','物业01','物业02','物业经营用房','发电机房','消防控制室','开闭所','生产车间','农贸市场','弱电机房','消控室','物业用房','不可售','不在项目内','洗手间','宿舍','物业','产品展示中心（工业）','度假屋','[商业]餐饮','[酒店]客房','[酒店]样板房','商务酒店','办公用房','其他']),'类别']="住宅"

jiage_sum = jiage.loc[jiage['建筑面积（㎡）'].notnull()&jiage['毛坯销售房屋总价（元）'].notnull(),['预售许可证', '项目名称','建筑面积（㎡）','毛坯销售房屋总价（元）','类别']].groupby(by=['预售许可证', '项目名称','类别'],as_index=False).sum()
jiage_sum['均价']=jiage_sum['毛坯销售房屋总价（元）']/jiage_sum['建筑面积（㎡）']
#利用单室价格的均价,匹配成交均价数据
a = pd.merge(sale,jiage_sum,how='left',left_on=['预售证号','项目名称','类别_2'],right_on=['预售许可证','项目名称','类别']).loc[:,['所在地区','预售证号','项目名称','套数','建筑面积','日期','均价_y','类别_2']].rename(columns={'均价_y':'均价','预售证号':'预售许可证号'})
#利用伯乐百家提供数据,匹配板块和项目推广名
b = pd.merge(a,tuiguangming,how='left',left_on="预售许可证号",right_on="预售许可证号").loc[:,['所在地区','预售许可证号','项目名称','套数','建筑面积','日期','均价','项目推广名','类别_2']].rename(columns={"均价":"住宅均价(元/㎡)",'项目名称':'项目备案名'})

b.loc[b['建筑面积']/b['套数']<90,'面积段']="小于90㎡面积"
b.loc[(b['建筑面积']/b['套数']<=120)&(b['建筑面积']/b['套数']>=90),'面积段']="90-120㎡面积"
b.loc[(b['建筑面积']/b['套数']<=144)&(b['建筑面积']/b['套数']>120),'面积段']="120-144㎡面积"
b.loc[(b['建筑面积']/b['套数']<=180)&(b['建筑面积']/b['套数']>144),'面积段']="144-180㎡面积"
b.loc[b['建筑面积']/b['套数']>180,'面积段']="180㎡以上面积"
b = b[pd.notna(b['面积段'])]
b["住宅均价(元/㎡)"] = round(b["住宅均价(元/㎡)"].astype('float'),2)
b['项目备案名'] = b['项目备案名'].apply(lambda x:x.replace("（详细信息）",""))

b.to_sql('楼盘成交排行榜_python',con=engine,if_exists='replace',index=False,index_label=False,dtype={"所在地区":sqlalchemy.types.VARCHAR(length=255),"预售许可证号":sqlalchemy.types.VARCHAR(length=255),"套数":sqlalchemy.types.INTEGER(),"建筑面积":sqlalchemy.types.DECIMAL(10,2),"日期":sqlalchemy.types.DATE(),"住宅均价(元/㎡)":sqlalchemy.types.DECIMAL(10,2),"项目推广名":sqlalchemy.types.VARCHAR(length=255),"面积段":sqlalchemy.types.VARCHAR(length=255),"类别_2":sqlalchemy.types.VARCHAR(length=255),"项目备案名":sqlalchemy.types.VARCHAR(length=255)})

#供应数据匹配推广名及板块,历史供应数据_python
junjia_info = pd.read_sql("select * from 房管网_销售项目2层_已清洗",con=engine)
junjia_all = pd.merge(junjia,junjia_info.loc[:,["预售许可证号","项目名称","交付时间/竣工时间","物业公司","占地面积","总建筑体量","容积率","绿地率/绿化率","预售商品房"]],how="left",left_on=["预售许可证号","项目名称"],right_on=["预售许可证号","项目名称"]).drop_duplicates(["预售许可证号","项目名称"],keep="last")
gongyin_tuiguangming = pd.merge(junjia_all,tuiguangming,how="left",left_on="预售许可证号",right_on="预售许可证号").loc[:,['预售许可证号','所在地区','项目测算面积','项目名称','开盘日期','房源总量','待售房源','非销售房产','住宅均价(元/㎡)','商业均价(元/㎡)','项目推广名',"交付时间/竣工时间","物业公司","占地面积","总建筑体量","容积率","绿地率/绿化率","预售商品房"]]
gongyin_tuiguangming.loc[gongyin_tuiguangming['项目测算面积']/gongyin_tuiguangming['房源总量']<90,'面积段']="小于90㎡面积"
gongyin_tuiguangming.loc[(gongyin_tuiguangming['项目测算面积']/gongyin_tuiguangming['房源总量']<=120)&(gongyin_tuiguangming['项目测算面积']/gongyin_tuiguangming['房源总量']>=90),'面积段']="90-120㎡面积"
gongyin_tuiguangming.loc[(gongyin_tuiguangming['项目测算面积']/gongyin_tuiguangming['房源总量']<=144)&(gongyin_tuiguangming['项目测算面积']/gongyin_tuiguangming['房源总量']>120),'面积段']="120-144㎡面积"
gongyin_tuiguangming.loc[(gongyin_tuiguangming['项目测算面积']/gongyin_tuiguangming['房源总量']<=180)&(gongyin_tuiguangming['项目测算面积']/gongyin_tuiguangming['房源总量']>144),'面积段']="144-180㎡面积"
gongyin_tuiguangming.loc[gongyin_tuiguangming['项目测算面积']/gongyin_tuiguangming['房源总量']>180,'面积段']="180㎡以上面积"
gongyin_tuiguangming = gongyin_tuiguangming[pd.notna(gongyin_tuiguangming['面积段'])]

gongyin_tuiguangming.to_sql('历史供应数据_python',con=engine,if_exists='replace',index=False,index_label=False,dtype={"预售许可证号":sqlalchemy.types.VARCHAR(255),"所在地区":sqlalchemy.types.VARCHAR(255),"项目测算面积":sqlalchemy.types.DECIMAL(10,2),"项目名称":sqlalchemy.types.VARCHAR(255),"开盘日期":sqlalchemy.types.DATE(),"房源总量":sqlalchemy.types.INTEGER(),"待售房源":sqlalchemy.types.INTEGER(),"非销售房产":sqlalchemy.types.INTEGER(),"住宅均价(元/㎡)":sqlalchemy.types.DECIMAL(10,2),"商业均价(元/㎡)":sqlalchemy.types.DECIMAL(10,2),"项目推广名":sqlalchemy.types.VARCHAR(255),"面积段":sqlalchemy.types.VARCHAR(255),"交付时间/竣工时间":sqlalchemy.types.VARCHAR(255),"物业公司":sqlalchemy.types.VARCHAR(255),"占地面积":sqlalchemy.types.DECIMAL(15,3),"容积率":sqlalchemy.types.VARCHAR(255),"绿地率/绿化率":sqlalchemy.types.VARCHAR(255),"预售商品房":sqlalchemy.types.VARCHAR(255)})

#供销比
xiao = b.groupby(by=['日期','所在地区'],as_index=False).sum()
gong = junjia.groupby(by=['开盘日期','所在地区'],as_index=False).sum().rename(columns={"开盘日期":"日期"})
g_x = pd.merge(xiao,gong,how='outer',left_on=['日期','所在地区'],right_on=['日期','所在地区']).loc[:,['日期','所在地区','套数','房源总量','待售房源','非销售房产']]

g_x.to_sql('各区域供销比_python',con=engine,if_exists='replace',index=False,index_label=False,dtype={"日期":sqlalchemy.types.DATE(),"所在地区":sqlalchemy.types.VARCHAR(255),"套数":sqlalchemy.types.INTEGER(),"待售房源":sqlalchemy.types.INTEGER(),"房源总量":sqlalchemy.types.INTEGER(),"非销售房产":sqlalchemy.types.INTEGER()})

#面积段
junjia.loc[junjia['项目测算面积']/junjia['房源总量']<90,'面积段']="小于90㎡面积"
junjia.loc[(junjia['项目测算面积']/junjia['房源总量']<=120)&(junjia['项目测算面积']/junjia['房源总量']>=90),'面积段']="90-120㎡面积"
junjia.loc[(junjia['项目测算面积']/junjia['房源总量']<=144)&(junjia['项目测算面积']/junjia['房源总量']>120),'面积段']="120-144㎡面积"
junjia.loc[(junjia['项目测算面积']/junjia['房源总量']<=180)&(junjia['项目测算面积']/junjia['房源总量']>144),'面积段']="144-180㎡面积"
junjia.loc[junjia['项目测算面积']/junjia['房源总量']>180,'面积段']="180㎡以上面积"
junjia = junjia[pd.notna(junjia['面积段'])]
xiao_2 = b.groupby(by=['日期','所在地区','面积段'],as_index=False).sum()
gong_2 = junjia.groupby(by=['开盘日期','所在地区','面积段'],as_index=False).sum().rename(columns={"开盘日期":"日期"})
g_x_2 = pd.merge(xiao_2,gong_2,how='outer',left_on=['日期','所在地区','面积段'],right_on=['日期','所在地区','面积段']).loc[:,['日期','所在地区','面积段','套数','房源总量','待售房源','非销售房产']]

g_x_2.to_sql('各区域供销比_面积段_python',con=engine,if_exists='replace',index=False,index_label=False,dtype={"日期":sqlalchemy.types.DATE(),"所在地区":sqlalchemy.types.VARCHAR(255),"面积段":sqlalchemy.types.VARCHAR(255),"套数":sqlalchemy.types.INTEGER(),"房源总量":sqlalchemy.types.INTEGER(),"待售房源":sqlalchemy.types.INTEGER(),"非销售房产":sqlalchemy.types.INTEGER()})

#库存数据
xiao_kucun = b.groupby(by=['日期','所在地区','面积段'],as_index=False).sum()
gong_kucun = junjia.groupby(by=['开盘日期','所在地区','面积段'],as_index=False).sum().rename(columns={"开盘日期":"日期"}).loc[:,['日期','所在地区','面积段','项目测算面积','房源总量','待售房源','非销售房产']]
gong_kucun['建筑面积'] = round(gong_kucun['待售房源']/gong_kucun['房源总量']*gong_kucun['项目测算面积'],2)
g_x_kucun = pd.merge(xiao_kucun,gong_kucun,how='outer',left_on=['日期','所在地区','面积段'],right_on=['日期','所在地区','面积段']).loc[:,['日期','所在地区','面积段','套数','建筑面积_x','待售房源','建筑面积_y','房源总量','非销售房产','项目测算面积']]

g_x_kucun.to_sql('各区域库存_python',con=engine,if_exists='replace',index=False,index_label=False,dtype={"日期":sqlalchemy.types.DATE(),"所在地区":sqlalchemy.types.VARCHAR(255),"面积段":sqlalchemy.types.VARCHAR(255),"套数":sqlalchemy.types.INTEGER(),"建筑面积_x":sqlalchemy.types.DECIMAL(10,2),"待售房源":sqlalchemy.types.INTEGER(),"建筑面积_y":sqlalchemy.types.DECIMAL(10,2),"房源总量":sqlalchemy.types.INTEGER(),"非销售房产":sqlalchemy.types.INTEGER(),"项目测算面积":sqlalchemy.types.DECIMAL(10,2)})

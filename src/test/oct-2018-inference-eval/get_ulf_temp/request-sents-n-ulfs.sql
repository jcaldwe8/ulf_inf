-- use with the command mysql -h ebdb-instance2.cuainl7oscs9.us-east-2.rds.amazonaws.com -P 3306 -u gkim21 -p ebdb --password < get_ulf_temp/request-sents-n-ulfs.sql > [out.txt]
  select la.sid, s.sentence, la.implicit_ops_added from layer_annotations la
  inner join (select max(id) id, sid
  	from layer_annotations
  	group by sid)
  unq on unq.id = la.id
  inner join sentences s on s.id = la.sid
where la.sid in (634889,923662,917533,931871,288801,923438,499757,1173551,931890,479291,741443,921672,839753,239696,927826,923734,921689,917597,903267,925796,923750,628842,921708,915565,915575,915578,919685,1163399,309612,917643,919712,1147045,1038502,647338,1382770,1335475,917685,932027,929981,917695,846022,614606,923858,917716,612566,1206513,1181949,792840,915159,971024,923922,250131,932119,928030,940327,926428,722994,915762,915764,588085,319799,557369,264507,1022273,927744,917837,526673,282967,786777,250218,932204,930162,915859,459128,917890,917893,915846,932233,917570,1081747,917910,743846,762281,612786,694713,600507,336316,682433,715205,932294,915912,395723,930255,638712,932312,728143,917987,259364,915947,917999,1161642,512510,569860,739853,694361,915994,614941,930338,633382,932393,926261,924218,920128,404037,926282,922191,938119,922205,1350239,467552,582759,432749,961136,1237619,266868,610934,930428,1364606,1174151,510600,1168018,922265,1190554,932507,932512,549541,385702,924328,916146,1429962,916161,916175,1344210,1149658,377295,928483,828134,916207,916208,924404,307973,1264399,1387296,998177,1227571,1012535,928568,922435,834373,824120,926547,893790,928607,932704,916324,406373,254824,486257,926578,723841,916361,926983,932752,924483,265114,922528,1207230,689066,920498,926196,914364,926662,922571,916836,760785,1414100,924630,916441,1006555,1176271,916455,1037295,932848,306167,877560,915626,930815,920607,916497,431123,676885,318494,1160225,556069,291879,930858,420702,924728,863289,1037496,453694,916557,932945,228437,1154134,1231961,443488,930914,519271,916328,930930,930067,915769,932995,926858,916997,754831,933009,666774,918688,924837,919794,926914,902341,1242313,928972,662734,916689,1184983,916699,1353452,634105,924923,320765,279809,662786,738563,1389829,920847,546064,933139,916757,714006,922903,1123611,683293,916776,711977,1154348,1174833,924981,918840,927033,875837,431423,920897,916805,924998,413019,583004,795997,931168,922983,279916,920942,931184,927094,932349,742796,595345,1209747,925077,1025432,933277,873889,1238437,931245,409009,929207,462269,562628,722384,648661,918998,963833,1326553,927203,1154539,697843,923124,679415,885332,923137,300549,730631,794121,933387,929301,927260,929081,914975,917034,234759,933423,605745,931384,280136,927311,917073,928810,919133,265147,390757,1089131,630388,360041,284282,1179264,928704,1097346,925315,923272,456983,921228,644749,926317,923282,358040,927385,1207968,947876,358062,915122,917175,919228,919157,1214146,279158,917191,923338,458444,923353,929498,915167,1055113,926184,345831,915189,657145,925435,381568,640773,1255177,925450,435994,448286,923428,927534,915250,927027,925497,915260,923453,491326,923466,649037,915284,585563,499557,601967,411507,915336,915347,931735,915352,915354,922607,769349,931744,378786,616355,554310,876454,925607,274359,333761,921543,866267,915422,1234912,931812,559083,1177589,657398,915447,1198073,1187838);
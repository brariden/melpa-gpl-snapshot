;;; ace-pinyin.el --- Jump to Chinese characters using ace-jump-mode or avy

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; URL: https://github.com/cute-jumper/ace-pinyin
;; Package-Version: 20160121.1643
;; Version: 0.2
;; Package-Requires: ((ace-jump-mode "2.0") (avy "0.2.0"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Demos: See https://github.com/cute-jumper/ace-pinyin

;;                              _____________

;;                                ACE-PINYIN

;;                               Junpeng Qiu
;;                              _____________


;; Table of Contents
;; _________________

;; 1 Setup
;; 2 Usage
;; 3 Traditional Chinese Characters Support
;; 4 Other available commands
;; .. 4.1 `ace-pinyin-dwim'
;; .. 4.2 `ace-pinyin-jump-word'
;; 5 Acknowledgment


;; Jump to Chinese characters using `ace-jump-mode' or `avy'.

;; UPDATE(2015-11-26): Now jumping to traditional Chinese characters is
;; supported by setting `ace-pinyin-simplified-chinese-only-p' to `nil'.


;; [[file:http://melpa.org/packages/ace-pinyin-badge.svg]]
;; http://melpa.org/#/ace-pinyin

;; [[file:http://stable.melpa.org/packages/ace-pinyin-badge.svg]]
;; http://stable.melpa.org/#/ace-pinyin


;; 1 Setup
;; =======

;;   ,----
;;   | (add-to-list 'load-path "/path/to/ace-pinyin.el")
;;   | (require 'ace-pinyin)
;;   `----

;;   Or install via [melpa].


;;   [melpa] http://melpa.org/#/ace-pinyin


;; 2 Usage
;; =======

;;   By default this package is using `ace-jump-mode'. When using
;;   `ace-jump-mode', the `ace-jump-char-mode' command can jump to Chinese
;;   characters. If you prefer `avy', you can make `ace-pinyin' use `avy'
;;   by:
;;   ,----
;;   | (setq ace-pinyin-use-avy t)
;;   `----

;;   When using `avy', `avy-goto-char', `avy-goto-char-2' and
;;   `avy-goto-char-in-line' are supported to jump to Chinese characters.

;;   Note `ace-pinyin-use-avy' variable should be set *BEFORE* you call
;;   `ace-pinyin-global-mode' or `turn-on-ace-pinyin-mode'.

;;   Example config to use `ace-pinyin' globally:
;;   ,----
;;   | ;; (setq ace-pinyin-use-avy t) ;; uncomment if you want to use `avy'
;;   | (ace-pinyin-global-mode +1)
;;   `----

;;   When the minor mode is enabled, then `ace-jump-char-mode' (or
;;   `avy-goto-char', depends on your config) will be able to jump to both
;;   Chinese and English characters. That is, you don't need remember an
;;   extra command or create extra key bindings in order to jump to Chinese
;;   character. Just enable the minor mode and use `ace-jump-char-mode' (or
;;   `avy-goto-char') to jump to Chinese characters.

;;   Besides, all other packages using `ace-jump-char-mode' (or
;;   `avy-goto-char') will also be able to jump to Chinese characters. For
;;   example, if you've installed [ace-jump-zap], it will also be able to
;;   zap to a Chinese character by the first letter of pinyin. Note
;;   `ace-jump-zap' is implemented by using `ace-jump-mode', so you can't
;;   use `avy' in this case. You can check out my fork of `ace-jump-zap'
;;   using `avy': [avy-zap].


;;   [ace-jump-zap] https://github.com/waymondo/ace-jump-zap

;;   [avy-zap] https://github.com/cute-jumper/avy-zap


;; 3 Traditional Chinese Characters Support
;; ========================================

;;   By default, `ace-pinyin' only supports simplified Chinese characters.
;;   You can make `ace-pinyin' aware of traditional Chinese characters by
;;   the following setting:
;;   ,----
;;   | (setq ace-pinyin-simplified-chinese-only-p)
;;   `----


;; 4 Other available commands
;; ==========================

;; 4.1 `ace-pinyin-dwim'
;; ~~~~~~~~~~~~~~~~~~~~~

;;   If called with no prefix, it can jump to both Chinese characters and
;;   English letters. If called with prefix, it can only jump to Chinese
;;   characters.


;; 4.2 `ace-pinyin-jump-word'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Using this command, you can jump to the start of a sequence of Chinese
;;   characters(/i.e./ Chinese word) by typing the sequence of the first
;;   letters of these character's pinyins. If called without prefix, this
;;   command will read user's input with a default timeout 1 second(You can
;;   customize the timeout value). If called with prefix, then it will read
;;   input from the minibuffer and starts search after you press "enter".


;; 5 Acknowledgment
;; ================

;;   - The ASCII char to Chinese character table(`ace-pinyin--simplified-char-table'
;;     in code) is from
;;     [https://github.com/redguardtoo/find-by-pinyin-dired].
;;   - @erstern adds the table for traditional Chinese characters.

;;; Code:

(require 'ace-jump-mode)
(require 'avy)

;; From `https://github.com/redguardtoo/find-by-pinyin-dired'
;; Author: redguardtoo(Chen Bin)
(defconst ace-pinyin--simplified-char-table
  '("[阿啊呵腌嗄锕吖爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯昂肮盎奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊]"
    "[把八吧巴爸罢拔叭芭霸靶扒疤跋坝笆耙粑灞茇菝魃岜捌钯鲅百白败摆伯拜柏呗掰捭佰稗办半版般班板伴搬扮斑颁瓣拌扳绊阪坂瘢钣舨癍帮邦棒膀榜傍绑磅谤浜梆镑蚌蒡报保包暴宝抱薄胞爆鲍饱堡曝刨褒豹雹苞葆褓孢煲鸨龅趵被北备背悲辈杯倍贝碑卑蓓惫悖狈呗焙鹎孛邶陂埤碚褙鐾鞴萆钡本奔笨苯夯畚贲锛坌崩甭绷蹦迸甏泵嘣蚌比必笔毕币避闭鼻彼逼壁臂弊碧鄙毙蔽庇匕璧敝陛弼篦婢愎痹妣濞铋裨俾髀萆薜哔狴庳秕滗舭毖吡嬖蓖贲畀荸埤筚箅芘襞跸荜编便边变遍辩辨贬鞭辫扁卞砭苄匾汴蝙笾碥窆褊弁鳊忭煸缏表标彪镖膘骠镳裱杓飙瘭髟飚婊飑鳔别憋瘪蹩鳖宾滨彬斌鬓缤殡濒槟摈膑傧玢豳髌镔并病兵冰饼丙柄秉炳禀邴摒波播博伯勃薄拨泊柏剥玻驳卜脖搏膊饽簸掰舶跛礴菠帛铂钵渤檗钹擘箔趵孛鹁踣亳啵不部布步补捕怖卜簿哺埔卟埠钸逋醭晡瓿钚]"
    "[擦嚓礤才采菜财材彩裁猜蔡踩睬参餐残惨灿惭掺蚕璨孱骖黪粲藏苍仓沧舱伧草操曹糙嘈槽螬艚漕策测侧厕册恻参岑涔曾层蹭噌查察差茶插刹叉诧茬碴喳岔嚓衩杈楂槎檫镲搽锸猹馇汊姹差柴拆豺钗侪虿瘥产颤缠禅蝉馋铲搀阐掺潺忏蟾婵谄谗廛孱澶冁躔蒇骣觇镡羼长场常唱厂尝昌肠偿畅倡倘敞怅娼猖嫦伥氅徜昶鲳阊菖苌鬯惝超朝潮炒吵抄嘲钞绰巢晁焯怊耖车彻撤扯澈掣坼砗称陈沉晨尘臣趁衬辰郴谶琛忱嗔伧抻谌宸榇龀碜成城程称承诚盛乘呈撑惩澄秤瞠橙噌逞铛丞骋埕枨塍铖裎酲柽蛏吃持迟尺赤斥池痴齿驰耻翅匙侈哧嗤啻弛蚩炽笞敕叱饬踟鸱褫豉坻墀茌篪傺媸螭彳眵魑瘛重冲充崇虫宠憧忡艟茺舂铳抽愁仇丑筹臭酬绸踌瞅惆畴稠雠俦帱瘳出处除初楚触储础厨畜躇橱雏矗怵锄杵搐绌黜褚蜍蹰刍滁楮憷亍樗揣啜踹嘬膪搋传穿川船串喘舛遄舡巛氚椽钏创窗床闯幢疮怆吹垂炊锤捶陲槌棰春纯唇蠢醇淳椿鹑蝽莼绰戳啜辍踔龊此次词差刺辞慈磁赐瓷兹茨雌祠疵呲鹚糍茈从匆聪丛葱囱琮淙枞苁骢璁凑楱辏腠促粗簇醋卒猝蹴蹙徂殂蔟酢攒窜篡蹿撺镩汆爨脆粹催摧崔萃翠瘁悴璀隹淬毳榱啐存村寸忖皴错措搓挫撮磋蹉矬嵯脞痤瘥鹾厝锉]"
    "[大打达答搭瘩嗒沓耷褡鞑笪靼怛妲哒疸代带待戴袋呆贷逮歹殆黛怠玳岱迨傣呔骀绐埭甙但单担弹淡旦蛋胆诞丹耽惮眈啖澹掸殚箪瘅赕疸聃氮萏郸儋当党荡档挡裆铛宕凼菪谠砀到道导倒岛刀悼盗蹈捣祷叨稻忉帱氘纛的得德锝等登灯邓凳瞪蹬噔磴戥镫簦嶝地第提底低帝弟敌抵递滴迪蒂堤笛缔涤嘀诋谛狄邸睇嫡翟砥娣棣荻羝坻柢觌骶氐绨镝碲籴嗲点电店典颠甸淀垫殿滇奠惦掂碘癫巅踮佃玷簟阽坫靛钿癜丶调掉吊雕刁钓凋叼貂碉铫铞鲷爹跌叠迭碟谍蝶喋佚踮牒耋蹀堞瓞揲垤鲽定订顶丁盯钉鼎叮町铤腚酊仃锭疔啶玎碇耵丢铥动东懂冬洞冻董栋咚恫侗氡硐鸫岽垌峒胨胴都斗豆抖逗兜陡窦蔸蚪篼痘都读度独毒督渡肚杜睹堵赌妒嘟渎笃牍镀犊黩髑椟芏蠹断段短端锻缎煅椴簖对队堆兑碓憝怼镦顿盾吨敦蹲钝炖遁盹沌囤墩趸镦礅砘多夺朵躲舵堕踱咄跺哆剁惰垛驮掇铎裰哚缍沲柁]"
    "[额俄恶饿哦鹅扼愕遏噩娥峨呃厄鄂讹婀蛾轭颚鳄锷谔屙锇阏垩腭苊鹗萼莪诶恩摁蒽而二儿尔耳迩饵洱鸸珥铒鲕贰佴]"
    "[发法罚乏伐阀砝筏垡珐反饭犯翻范凡烦返番贩繁泛帆藩幡梵樊燔蕃畈钒蘩矾蹯方放房访防仿芳妨纺彷坊肪舫钫鲂邡枋非费飞废肥啡沸菲肺匪诽腓扉吠霏绯妃斐翡蜚痱淝悱鲱篚芾狒镄榧分份纷奋愤粉氛芬坟焚粪忿吩汾棼鼢玢酚偾瀵鲼风封丰峰疯锋逢奉缝凤讽冯蜂枫烽俸砜唪酆葑沣佛否缶夫府服复父负副福富付妇附佛幅伏符赴腐浮扶腹抚覆肤赋弗傅辅拂甫俯斧缚咐脯袱俘敷阜芙釜孚腑匐孵辐涪讣氟桴蜉芾苻茯莩菔幞怫拊滏黼艴麸绂绋趺祓砩黻罘蚨跗蝠呋凫郛稃驸赙馥蝮鲋鳆]"
    "[咖尬嘎噶轧伽旮钆尕尜改该概盖丐钙赅溉垓陔戤感干敢赶甘肝杆尴赣橄竿秆擀坩苷柑泔矸澉疳酐淦绀旰刚港钢岗纲缸扛杠冈肛罡戆筻高告稿搞糕膏皋羔睾槁藁缟篙镐诰槔杲郜锆个革各歌格哥戈隔葛割阁胳搁疙咯鸽嗝骼颌屹搿膈镉纥袼仡鬲塥圪哿舸铬硌虼给根跟亘艮哏茛更耿耕颈庚羹梗哽赓鲠埂绠工公共供功攻宫贡恭巩躬龚弓拱肱汞蚣珙觥够购构狗沟勾苟钩觏篝垢佝岣诟鞲笱枸遘媾缑彀故古顾股鼓姑骨固孤谷估雇辜咕沽箍菇汩轱锢蛊梏鸪毂鹄臌瞽罟钴觚鹘菰蛄嘏诂崮酤牿牯痼鲴挂瓜刮寡呱褂卦剐鸹栝胍诖怪乖拐掴关观管官馆惯冠贯罐灌棺莞倌纶掼盥涫鳏鹳广光逛犷咣胱桄规归贵鬼桂跪柜轨瑰诡刽龟硅闺皈傀癸圭晷簋妫鲑匦庋宄炔刿桧炅鳜滚棍鲧绲磙辊衮国过果锅郭裹帼蝈聒馘掴埚虢呙崞猓椁蜾]"
    "[哈蛤铪还海孩害嘿咳亥骇骸嗨胲醢氦汉喊含寒汗韩憾涵函翰撼罕旱捍酣悍憨晗瀚鼾顸阚焊蚶焓颔菡撖邗邯行航巷杭夯沆颃绗珩好号毫豪浩耗皓嚎昊郝壕蒿貉灏镐嗥嚆薅濠蚝颢和何合河喝赫核吓贺盒呵禾荷鹤壑阂褐诃涸阖嗬貉曷颌劾盍纥蚵翮菏黑嘿嗨很恨狠痕横衡恒哼亨蘅珩桁红轰洪鸿哄宏虹弘烘泓闳薨讧蕻訇黉荭后候後厚侯喉吼猴逅糇骺堠瘊篌鲎乎护呼胡户湖忽互糊虎壶狐沪惚浒唬葫弧蝴囫瑚斛祜猢鹄醐戽扈唿笏琥滹鹕轷烀冱岵怙鹘槲觳瓠鹱煳话华化花划画滑哗桦猾砉铧骅怀坏徊淮槐踝欢换还环缓患幻唤宦焕痪寰鬟涣浣奂桓缳豢锾郇萑圜洹擐獾漶逭鲩黄皇荒晃慌煌惶恍谎璜徨簧凰幌潢蝗蟥遑隍肓磺癀湟篁鳇会回汇挥辉灰惠毁悔恢慧绘徽讳贿徊晦秽诲诙晖彗麾烩荟卉茴喙蛔恚洄珲蕙哕咴浍虺缋桧隳蟪婚混魂昏浑馄荤诨溷阍珲和或活火获货伙祸惑霍豁夥锪耠劐钬攉藿嚯镬蠖]"
    ""
    "[几给己机记及计即基济辑级极寄际技集纪击奇急激继既积籍鸡吉挤迹季寂绩疾饥祭缉忌剂圾姬矶肌嫉讥藉叽脊冀稽妓棘骥畸蓟汲悸岌伎笈跻瘠亟诘暨霁羁稷偈戟嵇楫唧鲫髻荠箕觊蒺畿虮齑殛墼佶掎芨丌麂蕺咭嵴芰笄哜洎乩戢屐剞跽玑鲚赍犄家加价假架甲佳驾夹嫁嘉贾稼茄佼挟颊皎侥枷珈戛迦伽浃痂胛笳荚葭钾镓嘏郏挢岬徼湫敫袈瘕恝铗袷蛱跏见间件建简坚监减渐检健兼剑艰肩键荐尖鉴剪践奸捡箭舰拣贱溅煎俭槛碱歼缄茧笺柬谏蹇僭涧菅謇硷睑锏饯毽鲣鞯蒹搛谫囝湔缣枧戬戋犍裥笕翦趼楗牮鹣腱踺将讲强江奖降蒋疆酱姜浆僵匠犟缰绛桨耩礓洚豇茳糨教交觉校叫较角脚焦骄郊轿搅嚼胶缴绞饺椒矫娇佼狡浇跤姣窖剿侥皎蕉酵礁鲛徼湫敫僬鹪峤蛟铰艽茭挢噍醮界解接结节街姐阶介借戒杰届皆捷截洁揭劫竭藉睫诫嗟拮孑碣秸诘桀芥偈颉讦疖疥婕羯鲒蚧骱喈进今金近尽仅紧禁劲津斤谨锦筋晋巾浸襟瑾矜靳缙烬噤觐馑堇衿荩廑妗卺赆槿经京精境警竟静惊景敬睛镜竞净井径晶荆兢颈憬靖鲸泾阱儆旌痉迳茎胫腈菁粳獍肼弪婧刭靓窘炯迥扃炅就九究酒久旧救纠揪疚舅韭赳鸠灸咎啾臼鹫阄僦厩玖柩桕鬏局据居句举具剧巨聚拒俱距惧菊拘矩桔驹鞠咀沮瞿锯炬飓趄掬踽踞遽橘倨疽龃屦犋裾钜苴雎鞫椐讵苣锔狙榘莒枸榉窭醵琚捐卷倦眷娟隽绢鹃涓镌锩鄄狷桊蠲觉绝决脚嚼掘诀崛爵抉倔獗嗟厥蹶攫谲矍撅噱孓橛噘珏桷劂爝镢蕨觖军均君俊峻钧隽筠菌郡骏竣麇皲捃浚]"
    "[卡咖喀咔佧胩开慨凯铠揩楷恺垲蒈锎剀锴忾看刊侃堪砍坎槛勘瞰龛阚莰戡抗康慷扛炕亢糠伉闶钪考靠铐烤拷犒栲尻可克科客刻课颗渴柯呵棵恪咳苛磕壳坷嗑瞌轲稞疴蝌溘髁钶窠颏珂岢骒缂氪锞蚵肯恳啃垦龈裉坑吭铿空恐控孔倥崆箜口扣抠寇叩蔻眍芤筘苦哭库裤酷枯窟骷刳堀喾绔夸跨垮挎胯侉会快块筷脍蒯哙侩狯浍郐款宽髋况狂矿框旷眶筐匡哐邝诓夼诳圹纩贶亏愧溃窥魁馈睽盔逵葵奎匮傀喟聩岿馗夔篑喹悝暌隗蒉蝰愦揆跬困昆捆坤鲲悃髡锟醌阃琨括阔扩廓栝蛞]"
    "[拉啦辣腊喇垃蜡剌邋旯瘌砬来赖莱睐癞籁徕涞赉铼崃濑兰蓝栏烂懒览滥拦篮揽澜榄婪缆斓岚阑褴镧罱谰漤浪狼朗郎廊琅螂榔啷莨锒稂阆蒗老劳牢捞姥佬潦唠烙酪涝崂痨醪铹栳铑耢了乐勒肋叻泐鳓仂类泪累雷蕾垒磊擂肋儡羸诔镭嘞檑嫘缧酹耒冷愣楞棱塄里理力利立李历离例礼丽励黎厉璃莉哩笠粒俐漓栗狸梨隶吏沥篱厘犁雳罹莅戾鲤俚砺藜俪蜊黧郦痢枥逦娌詈骊荔鳢喱鹂嫠蠡鬲鲡悝坜苈砾藓呖唳猁溧澧栎轹蓠傈缡疠疬蛎锂篥粝跞醴俩联连脸练恋怜莲廉炼帘链敛涟镰殓琏楝裢裣蠊鲢濂臁潋蔹奁两量良亮辆梁俩凉粮谅粱晾踉莨墚魉椋靓了料聊疗辽僚廖寥镣潦撩撂缭燎寮嘹钌獠鹩蓼尥列烈裂劣猎咧趔冽洌捩埒躐鬣林临邻琳淋霖麟凛吝鳞磷躏赁嶙辚檩遴粼蔺懔瞵啉膦廪领令另灵零龄凌玲铃陵岭拎伶聆囹棱菱翎苓瓴棂绫呤柃鲮酃泠羚蛉六留流陆刘溜柳碌瘤榴浏硫琉遛馏镏骝绺锍旒熘鎏鹨龙隆笼胧拢咙聋垄珑窿陇癃茏栊泷垅砻楼陋漏搂喽篓偻娄髅蝼镂蒌嵝耧瘘路陆录卢露鲁炉鹿碌庐芦噜颅禄辘卤虏麓泸赂漉戮簏轳鹭掳潞鲈撸栌垆胪蓼渌鸬逯璐辂橹镥舻氇律旅绿率虑履屡侣缕驴吕榈滤捋铝褛闾膂氯稆乱卵峦挛孪栾銮娈滦鸾脔略掠锊论轮伦沦仑抡囵纶落罗络洛逻裸骆萝螺锣箩摞烙捋珞骡猡镙椤倮蠃荦瘰泺漯脶硌雒]"
    "[呒马吗妈码麻骂嘛抹玛蚂蟆唛杩犸嬷买卖麦埋迈脉霾劢荬满慢漫曼蛮馒瞒蔓颟谩墁幔螨鞔鳗缦熳镘忙茫盲芒氓莽蟒邙漭硭毛冒猫贸矛帽貌茅茂髦卯耄瑁锚懋袤铆峁牦蟊泖昴茆旄蝥瞀么麽没美每妹眉梅媒枚魅煤昧霉玫媚寐糜袂酶莓嵋楣湄猸镅浼鹛镁们门闷扪懑焖钔梦蒙猛盟朦孟萌勐懵檬蠓瞢甍礞蜢虻艋艨锰密米秘迷弥谜觅眯蜜靡咪谧泌糜汨宓麋醚弭敉芈祢脒幂縻嘧蘼猕糸面免棉眠缅绵勉腼冕娩湎沔眄黾渑妙描秒庙苗渺瞄藐缪淼缈喵眇邈鹋杪灭蔑篾咩乜蠛民敏悯闽泯珉皿抿闵苠岷缗玟愍黾鳘名明命鸣铭冥茗溟酩瞑暝螟谬缪默莫模麽末磨摸摩寞漠墨抹魔陌嘿沫膜蓦蘑茉馍摹貉谟嫫秣镆殁瘼耱貊貘某谋眸缪鍪哞侔蛑目母木幕姆慕牧墓募暮牟亩穆睦拇沐牡仫坶苜毪钼]"
    "[嗯唔那拿呢哪纳娜呐捺钠镎肭衲乃奶奈耐氖艿鼐佴萘柰难南男楠喃囡囝腩蝻赧囊囔馕攮曩脑闹恼挠瑙淖呶猱铙孬硇蛲垴呢讷内馁嫩恁能嗯唔你呢尼泥逆倪匿拟腻妮霓昵溺旎睨鲵坭猊怩伲祢慝铌年念廿粘碾捻蔫撵拈黏鲶鲇辇埝娘酿鸟尿袅嬲茑脲捏涅聂孽蹑嗫啮镊镍乜陧颞臬蘖您恁宁凝拧泞咛狞柠佞聍苎甯牛纽扭妞钮拗忸狃农弄浓侬哝脓耨怒努奴弩驽胬孥女钕恧衄暖虐疟诺挪懦糯喏搦傩锘]"
    "[哦噢喔欧偶殴呕鸥讴瓯藕沤耦怄]"
    "[怕爬帕扒趴啪琶葩耙杷钯筢派排牌拍徘湃俳蒎哌判盘盼叛畔潘攀拚蹒磐爿蟠襻袢泮旁庞胖乓膀磅彷螃滂耪逄跑炮抛泡袍刨咆狍疱脬庖匏配陪培佩赔沛裴呸胚醅锫辔帔旆霈盆喷湓朋鹏碰彭捧棚蓬膨烹抨篷砰澎怦堋蟛嘭硼批否皮屁披疲辟啤脾匹僻劈譬坯痞癖琵毗霹噼媲郫裨纰丕鼙圮蚍蜱貔陂陴砒仳埤擗吡庀邳疋芘枇罴淠铍甓睥便片篇偏骗翩扁犏谝蹁骈缏胼票漂飘瓢嫖瞟骠嘌剽螵缥莩殍撇瞥氕丿苤品贫拼频聘拚姘嫔榀颦牝平评瓶凭萍乒屏苹坪枰娉俜鲆破迫颇婆坡泊泼魄粕珀叵攴钷笸钋陂泺鄱皤剖裒掊普铺扑朴谱浦葡蒲仆脯瀑菩溥匍璞噗圃埔氆镨蹼镤濮莆]"
    "[起其期气七奇妻企器汽棋齐旗弃启骑欺歧岂戚凄泣契琪乞祈漆迄脐栖沏祺崎祁琦蹊砌憩淇汔亟绮讫嘁岐萋俟杞芪荠耆槭颀芑屺欹桤綮萁蛴蜞綦鳍麒蕲柒亓骐葺畦圻碛恰洽掐伽袷葜髂前钱千签欠牵浅潜迁谦遣歉纤嵌乾谴铅虔钳骞倩堑黔掮悭芊缱愆荨芡阡佥搴褰肷钎仟犍钤岍箝鬈扦慊椠枪墙抢腔呛锵跄羌蔷戕襁樯炝蜣嫱锖戗羟镪桥悄乔巧侨瞧敲翘俏窍峭锹撬跷憔樵鞘橇诮愀谯荞峤缲硗鞒劁切且窃怯茄趄妾砌惬伽锲挈郄箧慊亲钦琴侵秦勤芹擒寝覃沁禽噙揿檎锓芩嗪螓衾廑溱吣情请青清轻晴庆倾卿擎顷氢罄蜻磬謦苘圊檠黥鲭氰箐綮穷琼穹茕邛蛩筇跫銎求球秋邱囚丘酋蚯裘俅虬鳅逑遒赇泅楸犰湫蝤巯鼽糗去取区曲趣屈趋驱渠躯娶觑瞿岖戌蛐衢蛆癯麴阒祛磲鸲诎蠼劬蕖蘧龋苣黢璩氍朐全权圈劝泉券拳犬诠颧蜷绻荃铨痊鬈辁悛畎醛筌却确缺雀瘸榷鹊阕阙炔悫群裙逡麇]"
    "[然染燃冉髯苒蚺让嚷攘壤瓤穰禳扰绕饶娆桡荛热惹喏人任认忍仁韧刃纫饪壬仞稔葚荏妊轫衽仍扔日容荣融蓉溶绒熔榕戎嵘茸冗肜蝾狨肉柔揉蹂鞣糅如入辱儒乳汝褥嚅茹濡蠕孺缛襦颥薷蓐洳溽铷软阮朊瑞锐芮睿蕤枘蕊蚋润闰若弱偌箬]"
    "[洒撒萨卅仨飒挲脎赛塞腮噻鳃三散伞叁毵馓糁霰丧桑嗓搡磉颡扫骚嫂梢臊搔缲缫鳋埽瘙色塞涩瑟啬铯穑森僧杀沙啥傻厦刹纱莎煞砂霎嗄挲歃鲨唼痧裟铩晒筛酾山善闪衫删煽扇陕珊杉擅掺膳栅讪跚汕姗赡潸缮嬗掸膻骟芟埏剡钐鄯舢苫髟疝蟮鳝上商伤尚赏殇裳晌觞熵墒绱垧少绍烧稍勺哨邵梢捎韶苕鞘潲劭杓芍蛸筲艄社设舍涉射摄舌蛇奢赦慑佘赊麝畲厍滠歙猞谁什身深神参甚申审沈伸慎渗绅肾呻婶莘蜃葚娠渖矧诜砷糁谂椹胂哂生声省胜升圣盛剩牲绳甥笙渑眚嵊晟是时十事实使世市识始士师诗式失史视示食室势试石释施适氏驶饰尸拾逝湿誓狮嗜蚀嘘屎侍匙峙仕恃柿轼矢噬拭虱弑蓍埘莳炻谥鲥豕贳铈螫舐筮鲺酾手受收首授守售瘦寿兽狩绶艏书数术属输树述熟束署殊舒叔鼠疏淑抒薯梳暑竖蜀恕墅孰漱枢俞赎黍蔬曙倏庶戍塾澍姝纾秫毹殳疋菽丨沭摅腧刷耍唰率衰摔甩帅蟀涮栓拴闩双爽霜孀泷水谁税睡顺舜瞬吮说朔硕烁铄妁蒴槊搠四死思斯司似私丝寺撕肆厮嘶伺饲嗣祀巳驷鸶俟汜泗厶兕蛳咝姒澌缌耜笥锶送松宋诵耸颂讼悚怂忪淞菘崧嵩凇竦搜艘嗽擞馊薮嗾叟嗖溲飕锼瞍螋苏诉速素俗肃宿塑稣溯酥粟簌夙嗉谡僳愫涑蔌觫算酸蒜狻岁随虽碎遂祟隧髓邃穗隋绥睢荽燧谇眭濉孙损笋荪狲飧榫隼所索缩锁琐梭嗦唆挲娑睃唢嗍蓑羧桫]"
    "[他她它踏塔塌榻嗒蹋沓遢挞鳎闼铊趿漯溻獭太台态泰抬胎汰苔呔鲐邰薹酞骀炱跆肽钛谈探弹坦叹坛摊贪滩毯谭潭瘫炭覃痰忐坍袒碳澹檀昙镡郯锬钽堂唐汤躺糖趟倘烫淌膛塘棠搪溏螳瑭樘螗铴醣镗耥饧傥帑羰讨套逃涛掏陶桃淘滔萄焘啕韬饕洮绦鼗特忑忒慝铽忒腾疼藤誊滕体提题替踢梯啼涕蹄剔剃惕屉嚏悌醍缇鹈锑荑倜绨逖裼天田填甜添腆舔恬钿阗畋忝殄掭条调跳挑迢眺鲦佻苕窕髫粜笤龆祧蜩铁贴帖餮萜听停庭厅挺亭婷廷艇町霆汀铤蜓莛梃葶烃同通统痛童彤筒铜桶捅桐瞳佟恸酮恫侗砼嗵仝垌茼峒潼头投偷透钭骰土突图途徒屠涂吐兔秃凸荼酴钍菟堍团湍抟疃彖推退腿褪颓蜕忒煺吞屯饨褪臀囤豚暾氽托脱拖妥拓陀驼唾椭砣驮沱跎坨鸵乇鼍橐佗庹铊酡柁柝箨]"
    ""
    ""
    "[瓦挖袜娃哇凹娲蛙洼佤腽外歪崴万完晚湾玩碗弯挽顽腕婉惋宛丸蜿莞畹剜豌皖纨琬脘烷芄菀绾望王往网忘亡汪旺枉妄惘罔尢辋魍为位未委维味围卫威微伟谓唯危慰尾违魏玮蔚伪畏胃喂炜韦惟巍纬萎娓苇尉帷渭猥偎薇痿猬逶帏韪煨鲔桅潍隈圩囗诿隗崴洧葳嵬闱沩涠艉軎文问闻温稳吻纹蚊雯紊瘟汶刎阌璺翁瓮嗡蓊蕹我握窝卧渥沃涡斡蜗幄喔倭挝莴肟硪龌无五物务武午舞於误恶吴屋伍悟吾污乌雾侮捂巫毋呜诬勿梧坞戊兀唔晤芜鹜钨妩痦鹉忤寤骛邬牾鼯圬浯仵阢芴庑婺怃杌焐蜈迕鋈]"
    "[西系息希喜席习细戏吸洗惜稀悉析夕牺袭昔熙兮溪隙嘻锡晰媳樨熄膝郗犀禧曦奚羲蹊唏淅嬉皙汐徙茜玺熹烯翕蟋屣檄浠僖穸蜥隰觋螅铣菥葸蓰舾矽粞硒醯欷鼷歙饩阋禊舄下夏吓峡厦侠狭霞瞎暇虾唬辖遐匣黠瑕呷狎柙硖瘕罅现先显线险限县鲜献闲宪陷贤仙嫌咸羡掀弦纤娴衔馅涎舷腺跣暹岘猃蚬筅跹莶锨鹇痫铣氙祆籼冼藓酰苋燹霰想相向象香乡像响项享降箱详祥巷厢湘橡翔镶飨襄饷骧葙庠鲞芗缃蟓小笑校消效晓销潇肖萧孝宵削嚣啸逍硝霄淆哮枭骁箫筱哓枵绡魈蛸崤些写谢协鞋携斜泄胁歇谐邪械屑卸挟懈泻亵蟹偕邂榭撷楔瀣蝎颉勰薤燮躞缬獬绁廨榍渫心新信欣辛薪馨鑫芯衅昕忻锌歆镡囟行性形兴星型姓幸刑醒腥杏悻惺邢猩荇擤荥饧硎陉雄兄胸凶熊匈汹芎修休秀袖宿臭羞绣朽锈嗅咻貅髹馐庥鸺岫溴许续需须徐序虚绪吁蓄叙畜嘘恤絮浒墟旭婿栩戌诩胥酗煦砉盱糈醑顼勖洫溆圩蓿选宣旋悬券喧轩玄炫渲绚眩萱漩暄璇谖铉儇痃泫煊楦癣碹揎镟学血雪削穴谑靴薛踅噱泶鳕寻询训迅讯巡逊循旬熏勋驯荤殉醺巽徇埙荀峋洵薰汛郇曛窨恂獯浔鲟蕈浚]"
    "[亚压雅牙呀押涯讶鸦哑鸭崖丫芽衙轧痖睚娅蚜伢疋岈琊垭揠迓桠氩砑眼言严演研烟验延沿掩颜厌炎燕阎宴盐咽岩雁焰艳焉淹衍阉奄谚俨檐蜒彦腌焱晏唁妍砚嫣胭湮筵堰赝餍鼹芫偃魇闫崦厣剡恹阏兖郾琰罨鄢谳滟阽鼽酽菸样洋阳央杨养扬仰羊痒漾泱氧鸯秧殃恙疡烊佯鞅怏徉炀蛘要摇药耀遥邀腰姚咬尧谣瑶窑夭肴妖吆钥侥杳窈鹞曜舀铫幺爻徭繇鳐珧轺崾也业夜爷叶野页液耶咽曳拽揶噎烨冶椰掖腋谒邺靥晔铘一以意已义议医易衣艺依译移异益亦亿疑遗忆宜椅伊仪谊抑翼矣役艾乙溢毅蛇裔逸姨夷轶怡蚁弈倚翌颐疫绎彝咦佚奕熠贻漪诣迤弋懿呓驿咿揖旖屹痍薏噫镒刈沂臆缢邑胰猗羿钇舣劓仡酏佾埸诒圯荑壹挹嶷饴嗌峄怿悒铱欹殪黟苡肄镱瘗癔翊蜴眙翳因音引印银隐饮阴姻瘾吟寅殷淫茵荫尹蚓垠喑湮胤鄞氤霪圻铟狺吲夤堙龈洇茚窨应英影营迎硬映赢盈颖鹰婴蝇樱莹荧膺萤萦莺罂瀛楹缨颍嬴鹦瑛茔嘤璎荥撄郢瘿蓥滢潆媵哟唷用永拥勇涌踊泳庸佣咏俑雍恿甬臃邕镛痈壅鳙饔喁墉蛹慵有又由友游右油优邮幽尤忧犹悠幼诱佑黝攸呦酉柚鱿莠囿鼬铀卣猷牖铕疣蚰蝣釉蝤繇莜侑莸宥蚴尢于与语育余遇狱雨於欲预予鱼玉愈域誉吁宇寓豫愚舆粥郁喻羽娱裕愉禹浴馀御逾渔渝俞萸瑜隅驭迂揄圄谕榆屿淤毓虞禺谀妪腴峪竽芋妤臾欤龉觎盂昱煜熨燠窬蝓嵛狳伛俣舁圉庾菀蓣饫阈鬻瘐窳雩瘀纡聿钰鹆鹬蜮员元原院远愿园源圆怨缘援冤袁渊苑猿鸳辕垣媛沅橼芫爰螈鼋眢圜鸢箢塬垸掾瑗月乐越约阅跃曰悦岳粤钥刖瀹栎樾龠钺运云允韵晕孕匀蕴酝筠芸耘陨纭殒愠氲狁熨郓恽昀韫郧]"
    "[杂扎砸咋咂匝拶在再载灾仔宰哉栽崽甾咱赞暂攒簪糌瓒拶昝趱錾藏脏葬赃臧锗奘驵早造遭糟澡灶躁噪凿枣皂燥蚤藻缲唣则责泽择咋啧仄迮笮箦舴帻赜昃贼怎谮增赠憎缯罾甑锃炸扎咋诈乍眨渣札栅轧闸榨喳揸柞楂哳吒铡砟齄咤痄蚱摘债宅窄斋寨翟砦瘵战展站占沾斩辗粘盏崭瞻绽蘸湛詹毡栈谵搌旃长张章丈掌涨帐障账胀仗杖彰璋蟑樟瘴漳嶂鄣獐仉幛嫜着找照招朝赵召罩兆昭肇沼诏钊啁棹笊这着者折哲浙遮辙辄谪蔗蛰褶鹧锗磔摺蜇赭柘真阵镇震针珍圳振诊枕斟贞侦赈甄臻箴疹砧桢缜畛轸胗稹祯浈溱蓁椹榛朕鸩政正证整争征挣郑症睁徵蒸怔筝拯铮峥狰诤鲭钲帧之只知至制直治指支志职致值织纸止质执智置址枝秩植旨滞徵帜稚挚汁掷殖芝吱肢脂峙侄窒蜘趾炙痔咫芷栉枳踯桎帙栀祉轾贽痣豸卮轵埴陟郅黹忮彘骘酯摭絷跖膣雉鸷胝蛭踬祗觯中种重众终钟忠衷肿仲锺踵盅冢忪舯螽周州洲粥舟皱骤轴宙咒昼肘帚胄纣诌绉妯碡啁荮籀繇酎主住注助著逐诸朱驻珠祝猪筑竹煮嘱柱烛铸株瞩蛛伫拄贮洙诛褚铢箸蛀茱炷躅竺杼翥渚潴麈槠橥苎侏瘃疰邾舳抓爪拽嘬传专转赚撰砖篆啭馔颛装状壮庄撞妆幢桩奘僮戆追坠缀锥赘隹椎惴骓缒准谆窀肫着桌捉卓琢灼酌拙浊濯茁啄斫镯涿焯浞倬禚诼擢子自字资咨紫滋仔姿吱兹孜梓渍籽姊恣滓谘龇秭呲辎锱眦笫髭淄茈觜訾缁耔鲻嵫赀孳粢趑总宗纵踪综棕粽鬃偬腙枞走奏邹揍驺鲰诹陬鄹组足族祖租阻卒诅俎镞菹赚钻攥纂躜缵最罪嘴醉咀觜蕞尊遵樽鳟撙作做坐座左昨琢佐凿撮柞嘬怍胙唑笮阼祚酢]")
  "ASCII char to simplifed Chinese characters.")

(defconst ace-pinyin--traditional-char-table
  '("[阿啊呵醃嗄錒吖愛哀挨礙埃癌艾唉矮哎皚藹隘曖靄捱噯璦嬡鎄嗌砹安案按暗岸俺諳黯鞍氨庵桉鵪胺銨揞犴垵昂骯盎奧澳傲熬敖凹襖懊坳嗷拗鏖驁鰲翱嶴廒遨獒聱媼螯鏊]"
    "[把八吧巴爸罷拔叭芭霸靶扒疤跋壩笆耙粑灞茇菝魃岜捌鈀鮁百白敗擺伯拜柏唄掰捭佰稗辦半版般班板伴搬扮斑頒瓣拌扳絆阪阪瘢鈑舨癍幫邦棒膀榜傍綁磅謗浜梆鎊蚌蒡報保包暴寶抱薄胞爆鮑飽堡曝刨褒豹雹苞葆褓孢煲鴇齙趵被北備背悲輩杯倍貝碑卑蓓憊悖狽唄焙鵯孛邶陂埤碚褙鐾鞴萆鋇本奔笨苯夯畚賁錛坌崩甭繃蹦迸甏泵嘣蚌比必筆畢幣避閉鼻彼逼壁臂弊碧鄙斃蔽庇匕璧敝陛弼篦婢愎痹妣濞鉍裨俾髀萆薜嗶狴庳秕潷舭毖吡嬖蓖賁畀荸埤篳箅芘襞蹕蓽編便邊變遍辯辨貶鞭辮扁卞砭苄匾汴蝙籩碥窆褊弁鯿忭煸緶表標彪鏢膘驃鑣裱杓飆瘭髟飈婊颮鰾別憋癟蹩鱉賓濱彬斌鬢繽殯瀕檳擯臏儐玢豳髕鑌並病兵冰餅丙柄秉炳稟邴摒波播博伯勃薄撥泊柏剝玻駁卜脖搏膊餑簸掰舶跛礴菠帛鉑鉢渤檗鈸擘箔趵孛鵓踣亳啵不部布步補捕怖卜簿哺埔卟埠鈽逋醭晡瓿鈈]"
    "[擦嚓礤才採菜財材彩裁猜蔡踩睬參餐殘慘燦慚摻蠶璨孱驂黲粲藏蒼倉滄艙傖草操曹糙嘈槽螬艚漕策測側廁冊惻參岑涔曾層蹭噌查察差茶插剎叉詫茬碴喳岔嚓衩杈楂槎檫鑔搽鍤猹餷汊奼差柴拆豺釵儕蠆瘥產顫纏禪蟬饞鏟攙闡摻潺懺蟾嬋諂讒廛孱澶囅躔蕆驏覘鐔羼長場常唱廠嘗昌腸償暢倡倘敞悵娼猖嫦倀氅徜昶鯧閶菖萇鬯惝超朝潮炒吵抄嘲鈔綽巢晁焯怊耖車徹撤扯澈掣坼硨稱陳沉晨塵臣趁襯辰郴讖琛忱嗔傖抻諶宸櫬齔磣成城程稱承誠盛乘呈撐懲澄秤瞠橙噌逞鐺丞騁埕棖塍鋮裎酲檉蟶吃持遲尺赤斥池癡齒馳恥翅匙侈哧嗤啻弛蚩熾笞敕叱飭踟鴟褫豉坻墀茌篪傺媸螭彳眵魑瘛重衝充崇蟲寵憧忡艟茺舂銃抽愁仇醜籌臭酬綢躊瞅惆疇稠讎儔幬瘳出處除初楚觸儲礎廚畜躇櫥雛矗怵鋤杵搐絀黜褚蜍躕芻滁楮憷亍樗揣啜踹嘬膪搋傳穿川船串喘舛遄舡巛氚椽釧創窗牀闖幢瘡愴吹垂炊錘捶陲槌棰春純脣蠢醇淳椿鶉蝽蓴綽戳啜輟踔齪此次詞差刺辭慈磁賜瓷茲茨雌祠疵呲鶿餈茈從匆聰叢蔥囪琮淙樅蓯驄璁湊楱輳腠促粗簇醋卒猝蹴蹙徂殂蔟酢攢竄篡躥攛鑹汆爨脆粹催摧崔萃翠瘁悴璀隹淬毳榱啐存村寸忖皴錯措搓挫撮磋蹉矬嵯脞痤瘥鹺厝銼]"
    "[大打達答搭瘩嗒沓耷褡韃笪靼怛妲噠疸代帶待戴袋呆貸逮歹殆黛怠玳岱迨傣呔駘紿埭甙但單擔彈淡旦蛋膽誕丹耽憚眈啖澹撣殫簞癉賧疸聃氮萏鄲儋當黨蕩檔擋襠鐺宕凼菪讜碭到道導倒島刀悼盜蹈搗禱叨稻忉幬氘纛的得德鍀等登燈鄧凳瞪蹬噔磴戥鐙簦嶝地第提底低帝弟敵抵遞滴迪蒂堤笛締滌嘀詆諦狄邸睇嫡翟砥娣棣荻羝坻柢覿骶氐綈鏑碲糴嗲點電店典顛甸澱墊殿滇奠惦掂碘癲巔踮佃玷簟阽坫靛鈿癜丶調掉吊雕刁釣凋叼貂碉銚銱鯛爹跌疊迭碟諜蝶喋佚踮牒耋蹀堞瓞揲垤鰈定訂頂丁盯釘鼎叮町鋌腚酊仃錠疔啶玎碇耵丟銩動東懂冬洞凍董棟咚恫侗氡硐鶇崬垌峒腖胴都鬥豆抖逗兜陡竇蔸蚪篼痘都讀度獨毒督渡肚杜睹堵賭妒嘟瀆篤牘鍍犢黷髑櫝芏蠹斷段短端鍛緞煅椴籪對隊堆兌碓憝懟鐓頓盾噸敦蹲鈍燉遁盹沌囤墩躉鐓礅砘多奪朵躲舵墮踱咄跺哆剁惰垛馱掇鐸裰哚綞沲柁]"
    "[額俄惡餓哦鵝扼愕遏噩娥峨呃厄鄂訛婀蛾軛顎鱷鍔諤屙鋨閼堊齶苊鶚萼莪誒恩摁蒽而二兒爾耳邇餌洱鴯珥鉺鮞貳佴]"
    "[發法罰乏伐閥砝筏垡琺反飯犯翻範凡煩返番販繁泛帆藩幡梵樊燔蕃畈釩蘩礬蹯方放房訪防仿芳妨紡彷坊肪舫鈁魴邡枋非費飛廢肥啡沸菲肺匪誹腓扉吠霏緋妃斐翡蜚痱淝悱鯡篚芾狒鐨榧分份紛奮憤粉氛芬墳焚糞忿吩汾棼鼢玢酚僨瀵鱝風封豐峯瘋鋒逢奉縫鳳諷馮蜂楓烽俸碸唪酆葑灃佛否缶夫府服復父負副福富付婦附佛幅伏符赴腐浮扶腹撫覆膚賦弗傅輔拂甫俯斧縛咐脯袱俘敷阜芙釜孚腑匐孵輻涪訃氟桴蜉芾苻茯莩菔襆怫拊滏黼艴麩紱紼趺祓砩黻罘蚨跗蝠呋鳧郛稃駙賻馥蝮鮒鰒]"
    "[咖尬嘎噶軋伽旮釓尕尜改該概蓋丐鈣賅溉垓陔戤感幹敢趕甘肝杆尷贛橄竿稈擀坩苷柑泔矸澉疳酐淦紺旰剛港鋼崗綱缸扛槓岡肛罡戇筻高告稿搞糕膏皋羔睾槁藁縞篙鎬誥槔杲郜鋯個革各歌格哥戈隔葛割閣胳擱疙咯鴿嗝骼頜屹搿膈鎘紇袼仡鬲塥圪哿舸鉻硌虼給根跟亙艮哏茛更耿耕頸庚羹梗哽賡鯁埂綆工公共供功攻宮貢恭鞏躬龔弓拱肱汞蚣珙觥夠購構狗溝勾苟鉤覯篝垢佝岣詬韝笱枸遘媾緱彀故古顧股鼓姑骨固孤谷估僱辜咕沽箍菇汩軲錮蠱梏鴣轂鵠臌瞽罟鈷觚鶻菰蛄嘏詁崮酤牿牯痼鯝掛瓜刮寡呱褂卦剮鴰栝胍詿怪乖拐摑關觀管官館慣冠貫罐灌棺莞倌綸摜盥涫鰥鸛廣光逛獷咣胱桄規歸貴鬼桂跪櫃軌瑰詭劊龜硅閨皈傀癸圭晷簋嬀鮭匭庋宄炔劌檜炅鱖滾棍鯀緄磙輥袞國過果鍋郭裹幗蟈聒馘摑堝虢咼崞猓槨蜾]"
    "[哈蛤鉿還海孩害嘿咳亥駭骸嗨胲醢氦漢喊含寒汗韓憾涵函翰撼罕旱捍酣悍憨晗瀚鼾頇闞焊蚶焓頷菡撖邗邯行航巷杭夯沆頏絎珩好號毫豪浩耗皓嚎昊郝壕蒿貉灝鎬嗥嚆薅濠蠔顥和何合河喝赫核嚇賀盒呵禾荷鶴壑閡褐訶涸闔嗬貉曷頜劾盍紇蚵翮菏黑嘿嗨很恨狠痕橫衡恆哼亨蘅珩桁紅轟洪鴻哄宏虹弘烘泓閎薨訌蕻訇黌葒後候後厚侯喉吼猴逅餱骺堠瘊篌鱟乎護呼胡戶湖忽互糊虎壺狐滬惚滸唬葫弧蝴囫瑚斛祜猢鵠醐戽扈唿笏琥滹鶘軤烀冱岵怙鶻槲觳瓠鸌煳話華化花劃畫滑譁樺猾砉鏵驊懷壞徊淮槐踝歡換還環緩患幻喚宦煥瘓寰鬟渙浣奐桓繯豢鍰郇萑圜洹擐獾漶逭鯇黃皇荒晃慌煌惶恍謊璜徨簧凰幌潢蝗蟥遑隍肓磺癀湟篁鰉會回匯揮輝灰惠毀悔恢慧繪徽諱賄徊晦穢誨詼暉彗麾燴薈卉茴喙蛔恚洄琿蕙噦咴澮虺繢檜隳蟪婚混魂昏渾餛葷諢溷閽琿和或活火獲貨夥禍惑霍豁夥鍃耠劐鈥攉藿嚯鑊蠖]"
    ""
    "[幾給己機記及計即基濟輯級極寄際技集紀擊奇急激繼既積籍雞吉擠跡季寂績疾飢祭緝忌劑圾姬磯肌嫉譏藉嘰脊冀稽妓棘驥畸薊汲悸岌伎笈躋瘠亟詰暨霽羈稷偈戟嵇楫唧鯽髻薺箕覬蒺畿蟣齏殛墼佶掎芨丌麂蕺咭嵴芰笄嚌洎乩戢屐剞跽璣鱭齎犄家加價假架甲佳駕夾嫁嘉賈稼茄佼挾頰皎僥枷珈戛迦伽浹痂胛笳莢葭鉀鎵嘏郟撟岬徼湫敫袈瘕恝鋏袷蛺跏見間件建簡堅監減漸檢健兼劍艱肩鍵薦尖鑑剪踐奸撿箭艦揀賤濺煎儉檻鹼殲緘繭箋柬諫蹇僭澗菅謇鹼瞼鐧餞毽鰹韉蒹搛譾囝湔縑梘戩戔犍襇筧翦趼楗牮鶼腱踺將講強江獎降蔣疆醬姜漿僵匠犟繮絳槳耩礓洚豇茳糨教交覺校叫較角腳焦驕郊轎攪嚼膠繳絞餃椒矯嬌佼狡澆跤姣窖剿僥皎蕉酵礁鮫徼湫敫僬鷦嶠蛟鉸艽茭撟噍醮界解接結節街姐階介借戒傑屆皆捷截潔揭劫竭藉睫誡嗟拮孑碣秸詰桀芥偈頡訐癤疥婕羯鮚蚧骱喈進今金近盡僅緊禁勁津斤謹錦筋晉巾浸襟瑾矜靳縉燼噤覲饉堇衿藎廑妗巹贐槿經京精境警竟靜驚景敬睛鏡競淨井徑晶荊兢頸憬靖鯨涇阱儆旌痙逕莖脛腈菁粳獍肼弳婧剄靚窘炯迥扃炅就九究酒久舊救糾揪疚舅韭赳鳩灸咎啾臼鷲鬮僦廄玖柩桕鬏局據居句舉具劇巨聚拒俱距懼菊拘矩桔駒鞠咀沮瞿鋸炬颶趄掬踽踞遽橘倨疽齟屨犋裾鉅苴雎鞫椐詎苣鋦狙榘莒枸櫸窶醵琚捐卷倦眷娟雋絹鵑涓鐫錈鄄狷桊蠲覺絕決腳嚼掘訣崛爵抉倔獗嗟厥蹶攫譎矍撅噱孓橛噘珏桷劂爝钁蕨觖軍均君俊峻鈞雋筠菌郡駿竣麇皸捃浚]"
    "[卡咖喀咔佧胩開慨凱鎧揩楷愷塏蒈鐦剴鍇愾看刊侃堪砍坎檻勘瞰龕闞莰戡抗康慷扛炕亢糠伉閌鈧考靠銬烤拷犒栲尻可克科客刻課顆渴柯呵棵恪咳苛磕殼坷嗑瞌軻稞痾蝌溘髁鈳窠頦珂岢騍緙氪錁蚵肯懇啃墾齦裉坑吭鏗空恐控孔倥崆箜口扣摳寇叩蔻瞘芤筘苦哭庫褲酷枯窟骷刳堀嚳絝誇跨垮挎胯侉會快塊筷膾蒯噲儈獪澮鄶款寬髖況狂礦框曠眶筐匡哐鄺誆夼誑壙纊貺虧愧潰窺魁饋睽盔逵葵奎匱傀喟聵巋馗夔簣喹悝暌隗蕢蝰憒揆跬困昆捆坤鯤悃髡錕醌閫琨括闊擴廓栝蛞]"
    "[拉啦辣臘喇垃蠟剌邋旯瘌砬來賴萊睞癩籟徠淶賚錸崍瀨蘭藍欄爛懶覽濫攔籃攬瀾欖婪纜斕嵐闌襤鑭罱讕灠浪狼朗郎廊琅螂榔啷莨鋃稂閬蒗老勞牢撈姥佬潦嘮烙酪澇嶗癆醪鐒栳銠耮了樂勒肋叻泐鰳仂類淚累雷蕾壘磊擂肋儡羸誄鐳嘞檑嫘縲酹耒冷愣楞棱塄裏理力利立李歷離例禮麗勵黎厲璃莉哩笠粒俐漓慄狸梨隸吏瀝籬釐犁靂罹蒞戾鯉俚礪藜儷蜊黧酈痢櫪邐娌詈驪荔鱧喱鸝嫠蠡鬲鱺悝壢藶礫蘚嚦唳猁溧澧櫟轢蘺傈縭癘癧蠣鋰篥糲躒醴倆聯連臉練戀憐蓮廉煉簾鏈斂漣鐮殮璉楝褳襝蠊鰱濂臁瀲蘞奩兩量良亮輛樑倆涼糧諒粱晾踉莨墚魎椋靚了料聊療遼僚廖寥鐐潦撩撂繚燎寮嘹釕獠鷯蓼尥列烈裂劣獵咧趔冽洌捩埒躐鬣林臨鄰琳淋霖麟凜吝鱗磷躪賃嶙轔檁遴粼藺懍瞵啉膦廩領令另靈零齡凌玲鈴陵嶺拎伶聆囹棱菱翎苓瓴欞綾呤柃鯪酃泠羚蛉六留流陸劉溜柳碌瘤榴瀏硫琉遛餾鎦騮綹鋶旒熘鎏鷚龍隆籠朧攏嚨聾壟瓏窿隴癃蘢櫳瀧壠礱樓陋漏摟嘍簍僂婁髏螻鏤蔞嶁耬瘻路陸錄盧露魯爐鹿碌廬蘆嚕顱祿轆滷虜麓瀘賂漉戮簏轤鷺擄潞鱸擼櫨壚臚蓼淥鸕逯璐輅櫓鑥艫氌律旅綠率慮履屢侶縷驢呂櫚濾捋鋁褸閭膂氯穭亂卵巒攣孿欒鑾孌灤鸞臠略掠鋝論輪倫淪侖掄圇綸落羅絡洛邏裸駱蘿螺鑼籮摞烙捋珞騾玀鏍欏倮蠃犖瘰濼漯腡硌雒]"
    "[嘸馬嗎媽碼麻罵嘛抹瑪螞蟆嘜榪獁嬤買賣麥埋邁脈霾勱蕒滿慢漫曼蠻饅瞞蔓顢謾墁幔蟎鞔鰻縵熳鏝忙茫盲芒氓莽蟒邙漭硭毛冒貓貿矛帽貌茅茂髦卯耄瑁錨懋袤鉚峁氂蟊泖昴茆旄蝥瞀麼麼沒美每妹眉梅媒枚魅煤昧黴玫媚寐糜袂酶莓嵋楣湄猸鎇浼鶥鎂們門悶捫懣燜鍆夢蒙猛盟朦孟萌勐懵檬蠓瞢甍礞蜢虻艋艨錳密米祕迷彌謎覓眯蜜靡咪謐泌糜汨宓麋醚弭敉羋禰脒冪縻嘧蘼獼糸面免棉眠緬綿勉靦冕娩湎沔眄黽澠妙描秒廟苗渺瞄藐繆淼緲喵眇邈鶓杪滅蔑篾咩乜蠛民敏憫閩泯珉皿抿閔苠岷緡玟愍黽鰵名明命鳴銘冥茗溟酩瞑暝螟謬繆默莫模麼末磨摸摩寞漠墨抹魔陌嘿沫膜驀蘑茉饃摹貉謨嫫秣鏌歿瘼耱貊貘某謀眸繆鍪哞侔蛑目母木幕姆慕牧墓募暮牟畝穆睦拇沐牡仫坶苜毪鉬]"
    "[嗯唔那拿呢哪納娜吶捺鈉鎿肭衲乃奶奈耐氖艿鼐佴萘柰難南男楠喃囡囝腩蝻赧囊囔饢攮曩腦鬧惱撓瑙淖呶猱鐃孬硇蟯堖呢訥內餒嫩恁能嗯唔你呢尼泥逆倪匿擬膩妮霓昵溺旎睨鯢坭猊怩伲禰慝鈮年念廿粘碾捻蔫攆拈黏鯰鯰輦埝娘釀鳥尿嫋嬲蔦脲捏涅聶孽躡囁齧鑷鎳乜隉顳臬櫱您恁寧凝擰濘嚀獰檸佞聹苧甯牛紐扭妞鈕拗忸狃農弄濃儂噥膿耨怒努奴弩駑胬孥女釹恧衄暖虐瘧諾挪懦糯喏搦儺鍩]"
    "[哦噢喔歐偶毆嘔鷗謳甌藕漚耦慪]"
    "[怕爬帕扒趴啪琶葩耙杷鈀筢派排牌拍徘湃俳蒎哌判盤盼叛畔潘攀拚蹣磐爿蟠襻袢泮旁龐胖乓膀磅彷螃滂耪逄跑炮拋泡袍刨咆狍皰脬庖匏配陪培佩賠沛裴呸胚醅錇轡帔旆霈盆噴湓朋鵬碰彭捧棚蓬膨烹抨篷砰澎怦堋蟛嘭硼批否皮屁披疲闢啤脾匹僻劈譬坯痞癖琵毗霹噼媲郫裨紕丕鼙圮蚍蜱貔陂陴砒仳埤擗吡庀邳疋芘枇羆淠鈹甓睥便片篇偏騙翩扁犏諞蹁駢緶胼票漂飄瓢嫖瞟驃嘌剽螵縹莩殍撇瞥氕丿苤品貧拼頻聘拚姘嬪榀顰牝平評瓶憑萍乒屏蘋坪枰娉俜鮃破迫頗婆坡泊潑魄粕珀叵攴鉕笸釙陂濼鄱皤剖裒掊普鋪撲樸譜浦葡蒲僕脯瀑菩溥匍璞噗圃埔氆鐠蹼鏷濮莆]"
    "[起其期氣七奇妻企器汽棋齊旗棄啓騎欺歧豈戚悽泣契琪乞祈漆迄臍棲沏祺崎祁琦蹊砌憩淇汔亟綺訖嘁岐萋俟杞芪薺耆槭頎芑屺欹榿綮萁蠐蜞綦鰭麒蘄柒亓騏葺畦圻磧恰洽掐伽袷葜髂前錢千籤欠牽淺潛遷謙遣歉纖嵌乾譴鉛虔鉗騫倩塹黔掮慳芊繾愆蕁芡阡僉搴褰肷釺仟犍鈐岍箝鬈扦慊槧槍牆搶腔嗆鏘蹌羌薔戕襁檣熗蜣嬙錆戧羥鏹橋悄喬巧僑瞧敲翹俏竅峭鍬撬蹺憔樵鞘橇誚愀譙蕎嶠繰磽鞽劁切且竊怯茄趄妾砌愜伽鍥挈郄篋慊親欽琴侵秦勤芹擒寢覃沁禽噙撳檎鋟芩嗪螓衾廑溱唚情請青清輕晴慶傾卿擎頃氫罄蜻磬謦檾圊檠黥鯖氰箐綮窮瓊穹煢邛蛩筇跫銎求球秋邱囚丘酋蚯裘俅虯鰍逑遒賕泅楸犰湫蝤巰鼽糗去取區曲趣屈趨驅渠軀娶覷瞿嶇戌蛐衢蛆癯麴闃祛磲鴝詘蠼劬蕖蘧齲苣黢璩氍朐全權圈勸泉券拳犬詮顴蜷綣荃銓痊鬈輇悛畎醛筌卻確缺雀瘸榷鵲闋闕炔愨羣裙逡麇]"
    "[然染燃冉髯苒蚺讓嚷攘壤瓤穰禳擾繞饒嬈橈蕘熱惹喏人任認忍仁韌刃紉飪壬仞稔葚荏妊軔衽仍扔日容榮融蓉溶絨熔榕戎嶸茸冗肜蠑狨肉柔揉蹂鞣糅如入辱儒乳汝褥嚅茹濡蠕孺縟襦顬薷蓐洳溽銣軟阮朊瑞銳芮睿蕤枘蕊蚋潤閏若弱偌箬]"
    "[灑撒薩卅仨颯挲脎賽塞腮噻鰓三散傘叄毿饊糝霰喪桑嗓搡磉顙掃騷嫂梢臊搔繰繅鰠埽瘙色塞澀瑟嗇銫穡森僧殺沙啥傻廈剎紗莎煞砂霎嗄挲歃鯊唼痧裟鎩曬篩釃山善閃衫刪煽扇陝珊杉擅摻膳柵訕跚汕姍贍潸繕嬗撣羶騸芟埏剡釤鄯舢苫髟疝蟮鱔上商傷尚賞殤裳晌觴熵墒鞝垧少紹燒稍勺哨邵梢捎韶苕鞘潲劭杓芍蛸筲艄社設舍涉射攝舌蛇奢赦懾佘賒麝畲厙灄歙猞誰什身深神參甚申審沈伸慎滲紳腎呻嬸莘蜃葚娠瀋矧詵砷糝諗椹胂哂生聲省勝升聖盛剩牲繩甥笙澠眚嵊晟是時十事實使世市識始士師詩式失史視示食室勢試石釋施適氏駛飾屍拾逝溼誓獅嗜蝕噓屎侍匙峙仕恃柿軾矢噬拭蝨弒蓍塒蒔炻諡鰣豕貰鈰螫舐筮鯴釃手受收首授守售瘦壽獸狩綬艏書數術屬輸樹述熟束署殊舒叔鼠疏淑抒薯梳暑豎蜀恕墅孰漱樞俞贖黍蔬曙倏庶戍塾澍姝紓秫毹殳疋菽丨沭攄腧刷耍唰率衰摔甩帥蟀涮栓拴閂雙爽霜孀瀧水誰稅睡順舜瞬吮說朔碩爍鑠妁蒴槊搠四死思斯司似私絲寺撕肆廝嘶伺飼嗣祀巳駟鷥俟汜泗厶兕螄噝姒澌緦耜笥鍶送鬆宋誦聳頌訟悚慫忪淞菘崧嵩凇竦搜艘嗽擻餿藪嗾叟嗖溲颼鎪瞍螋蘇訴速素俗肅宿塑穌溯酥粟簌夙嗉謖僳愫涑蔌觫算酸蒜狻歲隨雖碎遂祟隧髓邃穗隋綏睢荽燧誶眭濉孫損筍蓀猻飧榫隼所索縮鎖瑣梭嗦唆挲娑睃嗩嗍蓑羧桫]"
    "[他她它踏塔塌榻嗒蹋沓遢撻鰨闥鉈趿漯溻獺太臺態泰擡胎汰苔呔鮐邰薹酞駘炱跆肽鈦談探彈坦嘆壇攤貪灘毯譚潭癱炭覃痰忐坍袒碳澹檀曇鐔郯錟鉭堂唐湯躺糖趟倘燙淌膛塘棠搪溏螳瑭樘螗鐋醣鏜耥餳儻帑羰討套逃濤掏陶桃淘滔萄燾啕韜饕洮絛鞀特忑忒慝鋱忒騰疼藤謄滕體提題替踢梯啼涕蹄剔剃惕屜嚏悌醍緹鵜銻荑倜綈逖裼天田填甜添腆舔恬鈿闐畋忝殄掭條調跳挑迢眺鰷佻苕窕髫糶笤齠祧蜩鐵貼帖餮萜聽停庭廳挺亭婷廷艇町霆汀鋌蜓莛梃葶烴同通統痛童彤筒銅桶捅桐瞳佟慟酮恫侗砼嗵仝垌茼峒潼頭投偷透鈄骰土突圖途徒屠塗吐兔禿凸荼酴釷菟堍團湍摶疃彖推退腿褪頹蛻忒煺吞屯飩褪臀囤豚暾氽託脫拖妥拓陀駝唾橢砣馱沱跎坨鴕乇鼉橐佗庹鉈酡柁柝籜]"
    ""
    ""
    "[瓦挖襪娃哇凹媧蛙窪佤膃外歪崴萬完晚灣玩碗彎挽頑腕婉惋宛丸蜿莞畹剜豌皖紈琬脘烷芄菀綰望王往網忘亡汪旺枉妄惘罔尢輞魍爲位未委維味圍衛威微偉謂唯危慰尾違魏瑋蔚僞畏胃喂煒韋惟巍緯萎娓葦尉帷渭猥偎薇痿蝟逶幃韙煨鮪桅濰隈圩囗諉隗崴洧葳嵬闈潙潿艉軎文問聞溫穩吻紋蚊雯紊瘟汶刎閿璺翁甕嗡蓊蕹我握窩臥渥沃渦斡蝸幄喔倭撾萵肟硪齷無五物務武午舞於誤惡吳屋伍悟吾污烏霧侮捂巫毋嗚誣勿梧塢戊兀唔晤蕪鶩鎢嫵痦鵡忤寤騖鄔牾鼯圬浯仵阢芴廡婺憮杌焐蜈迕鋈]"
    "[西系息希喜席習細戲吸洗惜稀悉析夕犧襲昔熙兮溪隙嘻錫晰媳樨熄膝郗犀禧曦奚羲蹊唏淅嬉皙汐徙茜璽熹烯翕蟋屣檄浠僖穸蜥隰覡螅銑菥葸蓰舾矽粞硒醯欷鼷歙餼鬩禊舄下夏嚇峽廈俠狹霞瞎暇蝦唬轄遐匣黠瑕呷狎柙硤瘕罅現先顯線險限縣鮮獻閒憲陷賢仙嫌鹹羨掀弦纖嫻銜餡涎舷腺跣暹峴獫蜆筅躚薟杴鷳癇銑氙祆秈冼蘚醯莧燹霰想相向象香鄉像響項享降箱詳祥巷廂湘橡翔鑲饗襄餉驤葙庠鯗薌緗蟓小笑校消效曉銷瀟肖蕭孝宵削囂嘯逍硝霄淆哮梟驍簫筱嘵枵綃魈蛸崤些寫謝協鞋攜斜泄脅歇諧邪械屑卸挾懈瀉褻蟹偕邂榭擷楔瀣蠍頡勰薤燮躞纈獬紲廨榍渫心新信欣辛薪馨鑫芯釁昕忻鋅歆鐔囟行性形興星型姓幸刑醒腥杏悻惺邢猩荇擤滎餳硎陘雄兄胸兇熊匈洶芎修休秀袖宿臭羞繡朽鏽嗅咻貅髹饈庥鵂岫溴許續需須徐序虛緒籲蓄敘畜噓恤絮滸墟旭婿栩戌詡胥酗煦砉盱糈醑頊勖洫漵圩蓿選宣旋懸券喧軒玄炫渲絢眩萱漩暄璇諼鉉儇痃泫煊楦癬碹揎鏇學血雪削穴謔靴薛踅噱澩鱈尋詢訓迅訊巡遜循旬薰勳馴葷殉醺巽徇塤荀峋洵薰汛郇曛窨恂獯潯鱘蕈浚]"
    "[亞壓雅牙呀押涯訝鴉啞鴨崖丫芽衙軋瘂睚婭蚜伢疋岈琊埡揠迓椏氬砑眼言嚴演研煙驗延沿掩顏厭炎燕閻宴鹽咽巖雁焰豔焉淹衍閹奄諺儼檐蜒彥醃焱晏唁妍硯嫣胭湮筵堰贗饜鼴芫偃魘閆崦厴剡懨閼兗郾琰罨鄢讞灩阽鼽釅菸樣洋陽央楊養揚仰羊癢漾泱氧鴦秧殃恙瘍烊佯鞅怏徉煬蛘要搖藥耀遙邀腰姚咬堯謠瑤窯夭餚妖吆鑰僥杳窈鷂曜舀銚幺爻徭繇鰩珧軺崾也業夜爺葉野頁液耶咽曳拽揶噎燁冶椰掖腋謁鄴靨曄鋣一以意已義議醫易衣藝依譯移異益亦億疑遺憶宜椅伊儀誼抑翼矣役艾乙溢毅蛇裔逸姨夷軼怡蟻弈倚翌頤疫繹彝咦佚奕熠貽漪詣迤弋懿囈驛咿揖旖屹痍薏噫鎰刈沂臆縊邑胰猗羿釔艤劓仡酏佾埸詒圯荑壹挹嶷飴嗌嶧懌悒銥欹殪黟苡肄鐿瘞癔翊蜴眙翳因音引印銀隱飲陰姻癮吟寅殷淫茵蔭尹蚓垠喑湮胤鄞氤霪圻銦狺吲夤堙齦洇茚窨應英影營迎硬映贏盈穎鷹嬰蠅櫻瑩熒膺螢縈鶯罌瀛楹纓潁嬴鸚瑛塋嚶瓔滎攖郢癭鎣瀅瀠媵喲唷用永擁勇涌踊泳庸傭詠俑雍恿甬臃邕鏞癰壅鱅饔喁墉蛹慵有又由友遊右油優郵幽尤憂猶悠幼誘佑黝攸呦酉柚魷莠囿鼬鈾卣猷牖銪疣蚰蝣釉蝤繇莜侑蕕宥蚴尢於與語育餘遇獄雨於欲預予魚玉愈域譽籲宇寓豫愚輿粥鬱喻羽娛裕愉禹浴餘御逾漁渝俞萸瑜隅馭迂揄圄諭榆嶼淤毓虞禺諛嫗腴峪竽芋妤臾歟齬覦盂昱煜熨燠窬蝓嵛狳傴俁舁圉庾菀蕷飫閾鬻瘐窳雩瘀紆聿鈺鵒鷸蜮員元原院遠願園源圓怨緣援冤袁淵苑猿鴛轅垣媛沅櫞芫爰螈黿眢圜鳶箢塬垸掾瑗月樂越約閱躍曰悅嶽粵鑰刖瀹櫟樾龠鉞運雲允韻暈孕勻蘊醞筠芸耘隕紜殞慍氳狁熨鄆惲昀韞鄖]"
    "[雜扎砸咋咂匝拶在再載災仔宰哉栽崽甾咱贊暫攢簪糌瓚拶昝趲鏨藏髒葬贓臧鍺奘駔早造遭糟澡竈躁噪鑿棗皁燥蚤藻繰唣則責澤擇咋嘖仄迮笮簀舴幘賾昃賊怎譖增贈憎繒罾甑鋥炸扎咋詐乍眨渣札柵軋閘榨喳揸柞楂哳吒鍘砟齇吒痄蚱摘債宅窄齋寨翟砦瘵戰展站佔沾斬輾粘盞嶄瞻綻蘸湛詹氈棧譫搌旃長張章丈掌漲帳障賬脹仗杖彰璋蟑樟瘴漳嶂鄣獐仉幛嫜着找照招朝趙召罩兆昭肇沼詔釗啁棹笊這着者折哲浙遮轍輒謫蔗蟄褶鷓鍺磔摺蜇赭柘真陣鎮震針珍圳振診枕斟貞偵賑甄臻箴疹砧楨縝畛軫胗稹禎湞溱蓁椹榛朕鴆政正證整爭徵掙鄭症睜徵蒸怔箏拯錚崢猙諍鯖鉦幀之只知至制直治指支志職致值織紙止質執智置址枝秩植旨滯徵幟稚摯汁擲殖芝吱肢脂峙侄窒蜘趾炙痔咫芷櫛枳躑桎帙梔祉輊贄痣豸卮軹埴陟郅黹忮彘騭酯摭縶跖膣雉鷙胝蛭躓祗觶中種重衆終鍾忠衷腫仲鍾踵盅冢忪舯螽周州洲粥舟皺驟軸宙咒晝肘帚胄紂謅縐妯碡啁葤籀繇酎主住注助著逐諸朱駐珠祝豬築竹煮囑柱燭鑄株矚蛛佇拄貯洙誅褚銖箸蛀茱炷躅竺杼翥渚瀦麈櫧櫫苧侏瘃疰邾舳抓爪拽嘬傳專轉賺撰磚篆囀饌顓裝狀壯莊撞妝幢樁奘僮戇追墜綴錐贅隹椎惴騅縋準諄窀肫着桌捉卓琢灼酌拙濁濯茁啄斫鐲涿焯浞倬禚諑擢子自字資諮紫滋仔姿吱茲孜梓漬籽姊恣滓諮齜秭呲輜錙眥笫髭淄茈觜訾緇耔鯔嵫貲孳粢趑總宗縱蹤綜棕糉鬃傯腙樅走奏鄒揍騶鯫諏陬鄹組足族祖租阻卒詛俎鏃菹賺鑽攥纂躦纘最罪嘴醉咀觜蕞尊遵樽鱒撙作做坐座左昨琢佐鑿撮柞嘬怍胙唑笮阼祚酢]")
  "ASCII char to traditional Chinese characters.
Translated from ace-pinyin, powered by OpenCC.  Thanks to BYVoid.")

(define-obsolete-variable-alias 'ace-pinyin--char-table 'ace-pinyin--simplified-char-table "20151126")

(defgroup ace-pinyin nil
  "Make `ace-jump-char-mode' capable of jumping to Chinese characters"
  :group 'ace-jump-mode)

(defcustom ace-pinyin--jump-word-timeout 1
  "Seconds to wait for input."
  :type 'number
  :group 'ace-pinyin)

(defvar ace-pinyin-use-avy nil
  "Use `avy' or `ace-jump-mode'.
Default value is to use `ace-jump-mode'.")

(defvar ace-pinyin-simplified-chinese-only-p t
  "Whether `ace-pinyin' should use only simplified Chinese or not.
Default value is only using simplified Chinese characters.")

(defvar ace-pinyin--original-ace (symbol-function 'ace-jump-char-mode)
  "Original definition of `ace-jump-char-mode'.")

(defvar ace-pinyin--original-avy (symbol-function 'avy-goto-char)
  "Original definition of `avy-goto-char'.")

(defvar ace-pinyin--original-avy-2 (symbol-function 'avy-goto-char-2)
  "Original definition of `avy-goto-char-2'.")

(defvar ace-pinyin--original-avy-in-line (symbol-function 'avy-goto-char-in-line)
  "Original definition of `avy-goto-char-in-line'.")

(defun ace-pinyin--build-regexp (query-char &optional prefix)
  (let ((diff (- query-char ?a)))
    (if (and (< diff 26) (>= diff 0))
        (let ((regexp (nth diff
                           (if ace-pinyin-simplified-chinese-only-p
                               ace-pinyin--simplified-char-table
                             ace-pinyin--traditional-char-table))))
          (if prefix regexp
            (concat (format "[%c]" query-char)
                    (unless (string= regexp "") "\\|")
                    regexp)))
      (if (= 13 query-char)
          "\n"
        (regexp-quote (make-string 1 query-char))))))

(defun ace-pinyin--jump-impl (query-char &optional prefix)
  "Internal implementation of `ace-pinyin-jump-char'."
  (let ((regexp (ace-pinyin--build-regexp query-char prefix)))
    (if ace-pinyin-use-avy
        (avy-with avy-goto-char
          (avy--generic-jump regexp nil avy-style))
      (if ace-jump-current-mode (ace-jump-done))
      (if (eq (ace-jump-char-category query-char) 'other)
          (error "[AceJump] Non-printable character"))
      ;; others : digit , alpha, punc
      (setq ace-jump-query-char query-char)
      (setq ace-jump-current-mode 'ace-jump-char-mode)
      (ace-jump-do regexp))))

(defun ace-pinyin-jump-char (query-char)
  "AceJump with pinyin by QUERY-CHAR."
  (interactive (list (if ace-pinyin-use-avy
                         (read-char "char: ")
                       (read-char "Query Char:"))))
  (cond (ace-pinyin-mode
         (ace-pinyin--jump-impl query-char))
        (ace-pinyin-use-avy
         (funcall ace-pinyin--original-avy query-char))
        (t
         (funcall ace-pinyin--original-ace query-char))))

(defun ace-pinyin-jump-char-2 (char1 char2 &optional arg)
  "Ace-pinyin replacement of `avy-goto-char-2'."
  (interactive (list (read-char "char 1: ")
                     (read-char "char 2: ")
                     current-prefix-arg))
  (avy-with avy-goto-char-2
    (avy--generic-jump
     (concat "\\("
             (mapconcat #'ace-pinyin--build-regexp (list char1 char2) "\\)\\(")
             "\\)")
     arg
     avy-style)))

(defun ace-pinyin-jump-char-in-line (char)
  "Ace-pinyn replacement of `avy-goto-char-in-line'."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy--generic-jump
     (ace-pinyin--build-regexp char nil)
     avy-all-windows
     avy-style
     (line-beginning-position)
     (line-end-position))))

(defun ace-pinyin--jump-word-1 (query)
  (let ((regexp
         (mapconcat (lambda (char) (nth (- char ?a)
                                    (if ace-pinyin-simplified-chinese-only-p
                                        ace-pinyin--simplified-char-table
                                      ace-pinyin--traditional-char-table)))
                    query "")))
    (if ace-pinyin-use-avy
        (avy-with avy-goto-char
          (avy--generic-jump regexp nil avy-style))
      (if ace-jump-current-mode (ace-jump-done))

      (let ((case-fold-search nil))
        (when (string-match-p "[^a-z]" query)
          (error "[AcePinyin] Non-lower case character")))

      (setq ace-jump-current-mode 'ace-jump-char-mode)
      (ace-jump-do regexp))))

;;;###autoload
(defun ace-pinyin-jump-word (arg)
  "Jump to Chinese word.
If ARG is non-nil, read input from Minibuffer."
  (interactive "P")
  (if arg
      ;; Read input from minibuffer
      (ace-pinyin--jump-word-1 (read-string "Query Word: "))
    ;; Read input by using timer
    (message "Query word: ")
    (let (char string)
      (while (setq char (read-char nil nil ace-pinyin--jump-word-timeout))
        (setq string (concat string (char-to-string char)))
        (message (concat "Query word: " string)))
      (if string
          (ace-pinyin--jump-word-1 string)
        (error "[AcePinyin] Empty input, timeout")))))

;;;###autoload
(defun ace-pinyin-dwim (&optional prefix)
  "With PREFIX, only search Chinese.
Without PREFIX, search both Chinese and English."
  (interactive "P")
  (let ((query-char (if ace-pinyin-use-avy
                        (read-char "char: ")
                      (read-char "Query Char:"))))
    (ace-pinyin--jump-impl query-char prefix)))

;;;###autoload
(define-minor-mode ace-pinyin-mode
  "Toggle `ace-pinyin-mode'."
  nil
  " AcePY"
  :group ace-pinyin
  (if ace-pinyin-mode
      (if ace-pinyin-use-avy
          (progn
            (fset 'avy-goto-char 'ace-pinyin-jump-char)
            (fset 'avy-goto-char-2 'ace-pinyin-jump-char-2)
            (fset 'avy-goto-char-in-line 'ace-pinyin-jump-char-in-line))
        (fset 'ace-jump-char-mode 'ace-pinyin-jump-char))
    (if ace-pinyin-use-avy
        (progn
          (fset 'avy-goto-char ace-pinyin--original-avy)
          (fset 'avy-goto-char-2 ace-pinyin--original-avy-2)
          (fset 'avy-goto-char-in-line ace-pinyin--original-avy-in-line))
      (fset 'ace-jump-char-mode ace-pinyin--original-ace))))

;;;###autoload
(define-globalized-minor-mode ace-pinyin-global-mode
  ace-pinyin-mode
  turn-on-ace-pinyin-mode
  :group 'ace-pinyin
  :require 'ace-pinyin)

;;;###autoload
(defun turn-on-ace-pinyin-mode ()
  "Turn on `ace-pinyin-mode'."
  (interactive)
  (ace-pinyin-mode +1))

;;;###autoload
(defun turn-off-ace-pinyin-mode ()
  "Turn off `ace-pinyin-mode'."
  (interactive)
  (ace-pinyin-mode -1))

(provide 'ace-pinyin)
;;; ace-pinyin.el ends here

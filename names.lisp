
(in-package :vip-utils)

;; Some lists of names - useful for generating test data and anonymising data

;; https://en.wikipedia.org/wiki/List_of_Dune_characters

(defparameter *titles*
  '("Dr." "Duke" "Master" "Baron" "Tertius" "Sister" "Alma" "Count" "Lady" "Mr" "Mrs" "Miss" "Ms"
    "Father" "General" "Monsieur" "Tsar" "Prince" "Princess" "Lieutenant" "Mademoiselle"
    "Captain"
    "Staff-Captain" "Countess" "Archduke"
    "Reverend_Mother" "Emperor"
    ))


(defun get-names-from-list (list)
  "Get names as a list of first & other, surname where any except the
  surname can be nil. If there is only one list item we assume it's
  the surname since this is called after extracting the title."
  (setf list
        ;; this is necessary for 'von' in the name etc
        (mapcar (@ regex-replace-all "_" ?1 " ")
                list))
  (list (awhen (cdr list)
          (string-list (butlast list) " "))
        (first (last list))))

;; (get-names-from-list (list "Jessica"))
;; (get-names-from-list (list "John" "Sanderman" "Smith"))

(defun get-title-and-names (words)
  (cond ((member (first words) *titles* :test 'equal)
         (cons (regex-replace-all "_" (first words) " ")
               (get-names-from-list (cdr words))))
                      
        (t (cons nil (get-names-from-list words)))))

;; (get-title-and-names (list "Reverend_Mother" "Gaius" "Helen" "Mohiam"))
;; (get-title-and-names (list "Mr" "John" "Sanderman" "Smith"))

(defparameter *dune-characters*
  (loop for line in (remove-if (@ scan "^\\*" ?1) ; remove the section headers
                               (split "\\n" "* House Atreides
Arkie, Atreides House Trooper
Alia Atreides, Paul's younger sister
Ghanima Atreides, Paul's daughter and twin sister of Leto II
Ikonicre Atreides, majordomo to Leto II
Lady Jessica, Bene Gesserit and concubine of the Duke; mother of Paul and Alia
Duke Leto_I Atreides, head of House Atreides
Leto, first son of Paul and Chani; died as a toddler
Leto_II Atreides, Paul's son and twin brother of Ghanima
Moneo Atreides, majordomo and result of the God Emperor's breeding program; father of Siona Atreides
Paul Atreides, the Duke's son
Siona Atreides, daughter of Moneo
Gurney Halleck, staunchly loyal troubadour warrior of the Atreides
Thufir Hawat, Mentat and Master of Assassins to House Atreides
Duncan Idaho, Sword Master for House Atreides
Mattai, Atreides House Trooper
Dr. Wellington Yueh, Suk doctor for the Atreides
* House Harkonnen
Abulurd Harkonnen_II, half-brother of the Baron; father of Glossu and Feyd
Feyd-Rautha Rabban, nephew and heir of the Baron
Baron Vladimir Harkonnen, head of House Harkonnen
Umman Kudu, Former Guard Captain
Iakin Nefud, Guard Captain
Glossu 'Beast' Rabban, older nephew of the Baron
Piter De Vries, twisted Mentat
* Imperial House Corrino
Anirul, Bene Gesserit wife of Shaddam IV
Aramsham, Sardaukar Captain
Farad'n, son of Princess Wensicia and grandson of Shaddam IV
Princess Irulan, firstborn eldest daughter of Shaddam IV and Lady Anirul
Emperor Shaddam_IV, 81st Padishah Emperor of the Known Universe; last Corrino Emperor
Princess Wensicia, third daughter of Shaddam IV and Lady Anirul
Count Hasimir Fenring, Imperial Spice Minister; Emperor Shaddam's closest friend and advisor
Pardot Kynes, first Imperial Planetologist on Arrakis
Torynn, Sardaukar Levenbrech
* Bene Gesserit
Tertius Eileen Anteac
Bellonda
Sister Quintinius Violet Chenoeh
Lady Margot Fenring, wife of Count Hasimir Fenring
Marcus Claire Luyseyal
Reverend_Mother Gaius Helen Mohiam, the Emperor's Truthsayer and mentor of Lady Jessica.
Darwi Odrade
Schwangyu
Sheeana
Reverend_Mother Syaksa
Tamalane
Miles Teg
Alma Mavis Taraza
Wanna, wife of Dr. Wellington Yueh, Suk doctor
* Bene Tleilax
Master Bijaz
Duro Nunepi, Tleilaxu Ambassador to Leto II
Ledden Pook, Tleilaxu Envoy
Master Scytale
Master Tylwyth Waff
Wose, Tleilaxu Envoy
* Spacing Guild
Norma Cenva, inventor of foldspace technology; first Guild Navigator
Edric, Navigator in the events of Dune Messiah
Edrik, Navigator in the events of Hunters of Dune and Sandworms of Dune
D'murr Pilru, Guild Navigator, twin brother of C'tair Pilru of Ix
Aurelius Venport, founder of VenKee enterprises, the ancestor of the Guild
Adrien Venport, son of Aurelius Venport and Norma Cenva
* Honored Matres
Dama
Logno
Murbella
* Fremen
Chani, daughter of Liet-Kynes and Faroula; Paul's Fremen concubine
Chatt the Leaper, captain of the Fedaykin, leader of the death commandos who guard Muad'Dib
Dhuri, wife of Otheym
Farok
Faroula, daughter of Heinar, wife of Warrick and then Liet-Kynes
Geoff, killed by Jamis in ritual combat
Harah, wife of Jamis, the servant to Paul; later wife of Stilgar
Jamis, killed by Paul in ritual combat
Kaleff, natural son of Geoff
Korba, Fedaykin commando
Lichna, daughter of Otheym
Liet-Kynes, the son of Pardot Kynes, and the Imperial Planetologist on Arrakis
Orlop, natural son of Jamis
Otheym, Fedaykin commando
Reverend_Mother Ramallo, Fremen Reverend_Mother
Shadout Mapes, housekeeper for the royal family of Arrakis
Shishakli, a squad leader of the Fedaykin
Shoab, Tharthar's brother
Stilgar, Fremen naib, friend to Liet-Kynes
Tecrube, a Fremen and imperial clerk under Emperor Paul Atreides
Tharthar, one of Stilgar's wives
Turok, dies in a Harkonnen raid
* Miscellaneous
Abumojandia
Bannerjee, security officer under Emperor Paul Atreides
Hadi Benotto, archaeologist
Djedida, Korba´s secretary
Esmar Tuek, smuggler
Nayla, Fish Speaker
Tandis, Imperial messenger under Emperor Paul Atreides
Topri, spy and rebel under Siona Atreides
Ulot, Rebel under Siona Atreides"))
        ;; everything after the comma is a character description
        for name = (regex-replace "\\,.*" line "")
        for words = (split "\\s+" name)
        when (> (length words) 1)
          collect
          (get-title-and-names words)))

;; https://en.wikipedia.org/wiki/List_of_War_and_Peace_characters
(defparameter *war-and-peace*
  (loop for name in (mapcar (@ regex-replace " – .*" ?1 "")
                            (remove-if (@ eql (length ?1) 1)
                                       (split "\\n" "A
Stepan Stepanovich Adraksin – acquaintance of Pierre Bezukhov
Father Akinfi – monk and confessor of Marya Bolkonskaya.
Marya Dmitriyevna Akhrosimova – relative of Count Rostov and matchmaker. Strict but respected and admired.
Tsar Alexander I of Russia – liberal emperor early in his reign but gradually became more conservative.
Elizabeth Alexeievna – empress of Russia.
Yakov Alpatych – servant and estate manager of Prince Nikolay Bolkonsky
Count Aleksey Arakcheyev – severe minister of war in 1809; cruel but cowardly; former minister of war by 1812 but trusted by Tsar Alexander I
B
Karl Gustav von_Baggehufwudt – Russian general, killed at Tarutino
Prince Bagration – Russian general, considered \"The hero of heroes\" by Tolstoy. He is a modest, polite, but very strong character – An accurate image of Bagration in real life. Fought the French in a rear-guard action near Schoengraben in 1805, protecting Kutuzov. Commander of an army in 1812, killed at Borodino.
Balaga – troika driver for Anatole Kuragin.
Balashev – Adjutant-General in attendance upon the Tsar
Barclay de Tolly – Senior commander of Russian forces in 1812 until replaced by Kutuzov.
Barthélemy – The second envoy unsuccessfully sent by Napoléon to negotiate peace with Emperor Alexander.
Joseph Alexéevich Bazdéev – Pierre's benefactor, who introduced him to freemasonry.
Makar Alexeyevich Bazdeyev – brother of the above
de Beausset – Prefect of Napoleon's palace
Belliard – General in the French army at Borodino
Count Bennigsen – German leader of Russian at Eylau (a draw) and Friedland (a decisive defeat). A senior commander in 1812.
Lieutenant Alphonse Karlovich Berg – German husband of Vera Rostova
Berthier – Napoleon's commander of staff
Count Kirill Vladimirovich Bezukhov – Pierre's father and very wealthy aristocrat who served in Catherine II's court.
Pierre Bezukhov – The illegitimate son of Count Bezukhov. A freethinking, sometimes reckless, man capable of decisive action and great displays of willpower when circumstances demand it. Inherits Count Bezukhov's fortune, later becomes a Freemason and plans to assassinate Napoleon. Husband of Hélène Kuragina and after her death, of Natasha Rostova.
Bilibin – Russian diplomat to Austria. Appears in Vol I, Part II, Chapter 10. Entertains Prince Andrey Bolkonsky during the Prince's stay in Brno to inform the Austrian government of Russian victories.
Bitsky – \"a man who served on various committees and frequented all the different cliques of Petersburg\".
Maria Bogdanovna – midwife attending Princess Lisa Bolkonskaya
Bolkhovitinov – Messenger from Dolohov to Kutuzov, Oct. 1812
Prince Andrey Nikolayevich Bolkonsky – Son of Prince Nikolay Bolkonsky. A brave (at times arrogant) soldier who becomes cynical in the Napoleonic Wars. Counterpart to Pierre. Valued adjutant to Kutuzov in 1805. Married to Lisa Bolkonskaya, father of young prince Nikolay Bolkonsky, and afterwards engaged to Natasha Rostova.
Princess Elisabeta Karlovna Bolkonskaya – née Meinena. Wife of Andrey Bolkonsky. Also called \"little princess\".
Princess Marya Bolkonskaya – A woman who struggles between the obligations of her religion and the desires of her heart. Marya lives with her father at his estate, Bald Hills. She is subject to her father's fastidious and unscrupulous schedule and standards. Also called Maria. Eventually married Nikolai Rostov.
Prince Nikolay Andreevitch Bolkonsky – Name of both father and son of Prince Andrey Bolkonsky
Napoléon Bonaparte – The Great Man, ruined by great blunders.
Vincent Bosse – French drummer-boy, captured by Denisov
Mademoiselle Bourienne – orphaned French companion to Princess Marya Bolkonskaya and her father.
Broussier – French divisional commander
Captain Brozin – officer of the Russian army at Tarutino
Agrafena Ivanovna Byelova – a country neighbour of the Rostovs
C
General Campan
Caulaincourt – French ambassador to Russia
General Chatrov – an old comrade in arms of Prince Nikolai Bolkonsky
Pavel Vasilievich Chichagov –  or Tchichagov (8 July [O.S. 27 June] 1767 – 20 August 1849) – was a Russian military and naval commander of the Napoleonic wars.
Prince Adam Czartoryski – Minister of Foreign Affairs.
Clausewitz – As one of two German staff officers, in the Russian service, that ride past Prince Andrei the night of the eve of battle of Borodino (The other is Wolzogen).
D
Danilo – Huntsman for Nikolai Rostov
General Davout – French marshall, competent but also capable of cruelty
Vasily Denisov – Russian military officer, friend to Nikolai Rostov. He tends to pronounce some of his R's like Gh’s, almost like a Russian accent with English. Eventually a general of partisan troops during the French retreat from Moscow. Proposed unsuccessfully to Natasha Rostova.
Monsieur Dessalles – A Swiss teacher for young prince Nikolay Bolkonsky.
Lelorme d'Ideville – an interpreter
Dimmler – musician in the Rostov household
Dmitri Onufrich – Family solicitor of Count Bezukhov.
Dmitri Vasileyevich – \"Miten'ka.\" Account manager of the Rostovs.
Prince Dolgorukov – Russian general
Dmitry Dokhturov – One of the characters used as a mouthpiece by Tolstoy to express his disillusionment with the tendency of historians to attribute the course of events to the will of certain iconic, often heroic figures despite the fact that more obscure but perhaps equally influential characters contributed to the eventual outcome. Unheralded but played a decisive role at Austerlitz, Smolensk, Borodino, and Maley Yaroslavetz.
Fedor Ivanovich Dolokhov (Fedya) – Valiant in battle. A partisan leader in 1812. A cold man, he is a noted duelist and drinker, but is caring for his disadvantaged family. He once duels with Pierre and is nearly killed. Was rumored to be having an affair with Helene Bezukhov. Proposed unsuccessfully to Sonya. His possible prototypes were Count Fyodor Ivanovich Tolstoy, (also known as the \"American\"), Rufin Dorokhov (friend of Lermontov, killed during the Caucasian War), and renowned partisan leader Colonel Alexandre Figner.[1]
Maria Ivanovna Dolokhova – mother of Fedor Dolokhov
Dron Zakhárych – Village elder of Bogutcharovo
Princess Anna Mikhaylovna Drubetskaya – Friend of Countess Rostova. A poor, elderly lady. Supporter of Boris, her son.
Boris Drubetskoy – ambitious son of Princess Anna Mikhaylovna Drubetskaya. Army officer; fought at Austerlitz and later married Julie Karagina, thereby becoming rich. Childhood friend of Countess Rostova.
Dunyasha – Servant of Countess Rostova
E
Eykhen – officer of the General Staff at Tarutino
F
Colonel Fabvier – of the French army in Spain
Feoktist – \"famous head chef\" of the English Club
Maria Feodorovna (also Marya Fyodorovna) – Dowager empress of Russia
Archduke Ferdinand of_Austria
Filipp – footman to Prince Nikolai Bolkonsky
Emperor Francis_I of_Austria
Baron Funke
G
Prince Boris Vladimirovich Galitzine – A nobleman who has hired a tutor to instruct him in Russian, as French, the language preferred by the upper classes, became identified with the enemy.
Gavrilo – Maria Dmitrievna's \"gigantic footman\"
Gerasim – Servant to Bazdeyef
Gervais – Associate of Speranski
Glinka – editor of the Russian Messenger
Major-General Grekov – Commanded two regiments of cossacks under Orlov-Denisov at Taratino. Initially routed French under Marat.
H
Maria Hendrihovna – wife of the Russian army's regimental doctor
Hvostikov – friend of Anatole Kuragin
I
Ilyin – Friend of Nikolai Rostov, junior officer in the Army
Iogel – dancing master and organiser of balls in Moscow
Mikhail Ivanovich – Taciturn architect employed by Prince Nikolay Bolkonsky
J
Julner – colonel in Napoleon's army
K
Julie Karagina – wealthy heiress. Friend of Marya Bolkonskaya. Married Boris Drubetskoy.
Marya Lvovna Karagina – mother of Julie Karagina.
Platon Karataev – peasant who influences Pierre Bezukhov during his time as a prisoner of war.
Archduke Karl of_Austria
Andrei Sergeich Kaysarov – brother of Paisi Kaysarov
Paisi Kaysarov – Kutuzov's adjutant in the Battle of Borodino.
Kirsten – Staff-Captain who is listed as very honorable and proud of his regiment. He is said to have been demoted twice due \"affairs of honour,\" and has twice been reinstated to his current rank.
Count Kochubey – associate of Andrei Bolkonsky in St Petersburg (in Book 2 part 3)
Komarov – cossack with Petya Rostov in irregular forces
Kondratyevna – elderly housemaid in the Rostov household
Piotr Petrovich Konovnitsyn – Like Dokhturov, a character Tolstoy expresses his admiration of in order to reconcile the reader to the fact that the successful defense of Russia could not be achieved by those recognised by history alone.
Prince Kozlovsky – aide-de-camp to General Kutuzov (see below)
Aline Kuragina – Wife of Vasili Kuragin who only appears once in the novel.
Anatole Kuragin – son of Vasili Kuragin. Handsome, irresponsible and somewhat hedonistic military officer. Planned to seduce Natasha Rostova.
Hélène Kuragina – daughter of Vasili Kuragin. Later Countess Bezukhova (wife of Pierre Bezukhov). Beautiful, self-serving woman. Rumored at one point to have an affair with Fyodor Dolokhov.
Hippolyte Kuragin – (also Prince Ippolit) – son of Vasili Kuragin. A dull and boring man. A diplomat and the butt of Bilibin's humor.
Vasili Sergeevich Kuragin (also Prince Vassily) – self-seeking man who has a low opinion of his children but seeks to further their interests. Convinces Pierre Bezukhov to marry his daughter Hélène despite Pierre's reservations. Prince Vasili is self-serving and manipulative throughout the novel, and consistently attempts to swindle Pierre Bezukhov.
Mikhail Ilarionovich Kutuzov – real-life Russian general featuring throughout the book. His diligence and modesty eventually save Russia from French invasion.
Mavra Kuzminishna – elderly servant of the Rostovs.
L
Langeron – Noble who left France. A commander on the Russian side at Austerlitz, where his troops were decimated.
Larrey – surgeon to Napoleon
Lauriston – The first of two envoys sent to Kutuzov by Napoléon in an attempt to negotiate peace.
Lavrushka – Valet to Denisov. A rogue, later valet to Nikolai Rostov. Misled Napoleon.
Lazarev – Soldier at Kozlovski's battalion, was awarded a medal by Napoleon.
Lihachov – a Cossack in Denisov's guerilla force
Prince Lopuhin – dinner guest of Prince Nikolai Bolkonsky
Lorrain – Doctor present at the death of Count Bezukhov.
Esaul Lovaisky the Third – hetman Cossack with Denisov's irregulars
M
General Mack – Austrian general. Defeated at Ulm, 1805.
Magnitsky – Associate of Speransky, chairman of the Committee on Army Regulations.
Makarin – friend of Anatole Kuragin
Malasha – grand-daughter of Andrew Savyostayanov, six years old at the time of her appearance in 1812
Anna Ignatyevna Malvintsev – Princess Marya's aunt on her Mother's side, whose matchmaking abilities bring Nikolai Rostov and Marya together after she meets the prospective suitor at a soirée in Voronezh.
Princess Katerina Mamontova – one of Count Bezukhov's nieces. Eldest of the \"three princesses.\"
Princess Sophia Mamontova – one of Count Bezukhov's nieces. Youngest of the \"three princesses.\"
Matriona – a young Gypsy woman associated with Dolokhov
Mavra – a maid in the Rostov household.
Pelageya Danilovna Melyukova – a neighbour of the Rostovs
Métivier – French doctor fashionable in Moscow in 1811
Michaud – A Russian colonel. Brought news of the abandonment of Moscow to Tsar Alexander.
Mikhail Nikanorych – Distant relative of the Rostovs who lives near their estate at Otradnoe, he is also referred to as Uncle.
Miloradovich – Russian general in 1812 after Napoleon retreated from Moscow, previously Commander of a column at Austerlitz.
Mitka – Mikhail Nikanorych's coachman and good balalaika player.
Morel – orderly to Captain Ramballe
Abbé Morio – In the initial scene he is repeatedly referred to as 'the Abbé'; based on the real life priest and writer Scipione Piattoli.
Vicomte Mortemart – In the initial scene he is repeatedly referred to as 'the vicomte'.
General Mouton – The first Frenchman of consequence to explicitly accept that the best policy is to flee Russia.
Joachim Murat – French marshal, Napoleon's brother-in-law, styled as the King of Naples. With Napoleon in 1812 at Borodino. Retreated at Tarutino.
N
Nastasya Ivanovna – Cross-dressing \"old buffoon\" who lives with the Rostovs at their estate at Otradnoe.
Prince Nesvitsky – A Russian staff officer.
Michel Ney – French marshal. Fought at Borodino.
Novosiltsev – A Russian statesman and a close aide to Alexander I of Russia.
O
Count Orlov-Denisov – Commander of Cossacks who alone reached the assigned position at Taratino. His forces caused Murat to retreat.
Count Osterman-Tolstoy – Present at a council near Moscow during the retreat to beyond that city.
P
Maria Ignatyevna Peronskaya – Friend and relation of Countess Natalya Rostova.
Katerina Petrovna
Pfuel – German chief organizer of Russian Plan of Campaign in 1812. Contemptuous of other theorists.
Platov – Officer in whose division Nicholas Rostov was assigned.
Prokofy – footman in the Rostov household
R
Raevsky – Russian general at the middle of the action at Borodino.
Captain Ramballe – 13th Light Regiment, Chevalier of the Legion of Honor. Met Pierre Bezukhov in Moscow. Weak after Krasnoe.
Rapp – adjutant to Napoleon at Borodino.
Prince Repnin – Squadron commander of Russian army at Austerlitz.
Count Fiodor Vasilyevich Rostopchin – Governor-General of Moscow.
Count Ilya Rostov – Spendthrift. Optimistic father, agreeable but foolish.
Countess Natalya Rostova – Wife of Count Ilya.
Natasha Rostova – Initially, a romantic young girl, she evolves through trial and suffering, including engagement to Prince Bolkonsky which is terminated by her unfaithfulness, then later by his death,and eventually finds domestic happiness with Pierre Bezukhov.
Nikolai Rostov – The eldest Rostov son, who joins the Russian military in 1805. He eventually marries Princess Marya Bolkonskaya.
Petya Rostov – The youngest Rostov son. Becomes a soldier. Killed in a partisan raid.
Vera Rostova – The oldest Rostov daughter, she eventually marries Lieutenant Berg.
S
Praskovya Savishna – nurse in the Bolkonsky household
Anna Pavlovna Scherer – A wealthy St. Petersburg socialite. Unmarried hostess of patriotic circle.
Schmidt – Austrian general killed in battle at Krems, where Kutuzov won a victory.
Madame Schoss – associate of the Rostov household
Shapovalov – The Cossack who stumbled upon the left flank of Murat's army on October 2 while pursuing a hare and the inactivity he witnessed was sufficient evidence to support the Battle of Tarutino.
Shcherbinin – Gen. Konovnitsyn's adjutant in 1812.
Pyotr Nikolaitch Shinshin – relative of Countess Natalya Rostova. Famous for biting wit.
Smolyaninov – Freemason rhetor.
Sonya – The 'sterile flower'. Orphaned cousin of Vera, Nikolai, Natasha, and Petya Rostov. Engaged to Nikolai throughout most of the book, toward the end she releases him to marry Princess Marya.
Mihail Mihailovich Speransky – liberal advisor to the Tsar. Eventually dismissed by Tsar Alexander.
Stevens – An English naval officer, mentioned briefly early on in the novel.
Stolypin – Associate of Speranski.
Suhtelen – Lieutenant in Russian army wounded at Austerlitz
T
Semeon Tchekmar – Valet to Count Ilya Rostov
Lieutenant Telyanin – In Denisov's squadron early in the novel. Not well liked.
Theodosia – a religious pilgrim known to Maria Bolkonskaya.
Tikhon Shtcherbatov – Peasant scout with Denisov's partisan force.
Timohin – Officer who had a predilection for Bacchus. Valiant in battle.
Tishka
Captain von_Toll – Helped Alexander across a ditch after the rout of the Russian center at Austerlitz. A colonel in 1812.
Count Tolstoy – Grand marshal of the Russian court in 1805; Member of the Tsar's suite in 1812.
Staff-Captain Tushin – Commander of a battery of four cannon that fought valiantly and successfully at Schoengraben. Lost an arm at Friedland.
Tutolmin – A diplomat sent by Napoleon from Moscow to Alexander in Petersburg.
V
Vereshchagin – Name of Moscow merchant and his son. Son accused of treason and scapegoated by Count Rostopchin for loss of Moscow to the French, whereupon he was mutilated by a mob.
Prince Volkonsky – Member of the Tsar's suite in 1812.
Sergei Kuzmich Vyazmitinov
W
Weyrother – Austrian general who replaced Schmidt. Developed the plan of attack at Austerlitz.
Willarski – Pierre's sponsor, who delivers the formal invitation for him to join the Fraternity of Freemasons.
General Wintzingerode – German nobleman and officer in several different armies of the Napoleonic Wars.
Wolzogen – Implementer of Pfuhl's plan in 1812.
Y
Captain Yakovlev – Bearer of a message from Napoleon in Moscow to Alexander in Petersburg.
Yermolov – In Bagration's camp in 1812. Led an attack on Raevsky's redoubt as it was being overrun by the French. Later advised retreat from Fili that involved abandoning Moscow to the French.
Z
Zahar – Count Rostov's coachman.
Zdrzhinsky – an officer in the Russian army in 1812
Zherkov – A cornet of hussars who mimicked a general. Prone to jest.
Count Zhilinsky – Wealthy Polish count at Tilsit meeting of Napoleon and Alexander.")))
        for words = (split " " name)
        when (> (length words) 1)
          collect (get-title-and-names words)))


(defparameter *sample-names*
  (loop for name in (append *dune-characters*
                            *war-and-peace*)
        collect (loop for field in '(:title :other-names :surname)
                      for x in name
                      when x collect field
                        when x collect x)))

(defparameter *companies*
  (mapcar (@ strip-leading-and-trailing-space
             (first (split "\\t" ?1)))
          (split "\\n" " 33¢ Store                          	The Simpsons: No employee discount.                                                                                    	               
 Acme Corp.                         	Looney Tunes                                                                                                           	$348.7 billion 
 Adventureland                      	Adventureland: You can come of age and make a few extra bucks at this amusement park.                                  	               
 Arcade Flower Shop                 	Three’s Company: Never any misunderstandings.                                                                          	               
 Arnold’s Drive-In                  	Happy Days: Fonz rules!                                                                                                	               
 Atlantic American Airlines         	Meet the Parents: Chopsticks in your hair are optional.                                                                	               
 Average Joe’s Gym                  	Dodgeball: A True Underdog Story: A welcoming work environment, why not feel good and drop a few pounds in the process?	               
 Bada Bing                          	Sopranos: Boobalicious.                                                                                                	               
 Blue Moon Detective Agency         	Moonlighting: Old actors at their hottest.                                                                             	               
 Bluehound Bus Line                 	Dilbert                                                                                                                	               
 Bluth’s Original Frozen Banana     	Arrested Development: where there’s always money, there’s a good place to work!                                        	               
 Brawndo                            	Idiocracy: The chief reason that future generations become stupider, but still a thirst quencher like no other.        	               
 Burrito Explosion                  	Mr. Meaty: Cooler then Soy What?                                                                                       	               
 Bushwood Country Club              	Caddyshack: Everyday is Groundhog’s Day.                                                                               	               
 Buy-N-Large                        	Wall-E: Because they own everything in the world.                                                                      	               
 CHOAM                              	Dune                                                                                                                   	$1.7 trillion  
 Career Transitions Corporation     	Up In The Air: Telling people they’re fired all day long? For some, that probably sounds like heaven.                  	               
 Clampett Oil                       	Beverly Hillbillies                                                                                                    	$18.1 billion  
 Consumer Recreation Services       	The Game: Challenging and fun.                                                                                         	               
 Cyberdyne Systems Corp.            	Terminator                                                                                                             	$5.5 billion   
 Daily Planet                       	Superman: Unlimited sick days.                                                                                         	               
 Duff Beer                          	The Simpsons: You won’t be able to get enough, of that wonderful Duff.                                                 	               
 El Banco Corrupto                  	Grand Theft Auto: Vice City: Fill your pockets.                                                                        	               
 Entertainment 720                  	Parks & Recreation: You get paid to shoot free throws with Roy Hibbert and Detlef Schrempf.                            	               
 Ewing Oil                          	Dallas: That’s where the money is!                                                                                     	               
 Fisher & Sons                      	Six Feet Under: Come in everyday to ponder the meaning of life and death.                                              	               
 Flingers                           	Office Space: Flair!                                                                                                   	               
 Frobozz Magic Co.                  	Zork                                                                                                                   	$112.9 billion 
 Frosty Palace                      	Grease: Enjoy a milkshake while you work, just don’t mind it when the youth start breaking into song on you.           	               
 Ghostbusters                       	Ghostbusters: If you’re looking to fight the good fight, and ain’t afraid of no ghosts.                                	               
 Globex Corporation                 	The Simpsons: Sure, your boss is supervillain Hank Scorpio, but how about those hammocks!                              	               
 Goliath National Bank              	How I Met Your Mother: a lengen…wait for it…dary place to work!                                                        	               
 Grace Adler Designs                	Will & Grace: From what we can tell                                                                                    	               
 H.A.L. Labs                        	2001: A Space Odyssey: Damn Heuristically programmed Algorithmic computers!                                            	               
 Hudsucker Industries               	Hudsicker Proxy: From the mailroom to the top.                                                                         	               
 IPS (International Parcel Services)	King of Queens: What can crown do for you?                                                                             	               
 Lacuna Inc.                        	Eternal Sunshine: F with their minds.                                                                                  	               
 Life Extension                     	Vanilla Sky: Open your eyes.                                                                                           	               
 Livingston                         	Gentry & Mishkin, Bosom Buddies: Sure, you rarely ever get to see your boss, but your coworkers are great              	               
 Lomax Industries                   	Weekend at Bernie’s: The boss is dead!                                                                                 	               
 Los Pollos Hermanos                	Breaking Bad: The chicken might just be okay, but for some reason the benefits plan is stellar!                        	               
 M.I.B.                             	Men In Black: Defend the universe – we hear the benefits plan is pretty good, too.                                     	               
 Macmillan Toys                     	Big: Damn Zultan!                                                                                                      	               
 McDowell’s                         	Coming to America: The original Mickey D’s.                                                                            	               
 Mega Lo Mart                       	King Of The Hill: An added bonus if you’re a huge Chuck Mangione fan.                                                  	               
 Mel’s Diner                        	Mel’s Diner: Free cherry pie.                                                                                          	               
 Merrick BioTech                    	The Island: Clones of Scarlett and Jude?  Say no more.                                                                 	               
 Mitch and Murray                   	Glengarry Glen Ross: Better hope you don’t get third prize!                                                            	               
 Moe’s Tavern                       	The Simpsons: Hi, is I.P. Daily there?                                                                                 	               
 MomCorp                            	Futurama                                                                                                               	$291.8 billion 
 Monsters Inc.                      	Monsters Inc.: For those who enjoy working with children.                                                              	               
 Mugatu Industries                  	Zoolander: So hot right now…                                                                                           	               
 Multi National United              	District 9: Working with aliens all day, why not?                                                                      	               
 Nakatomi Trading Corp.             	Die-Hard                                                                                                               	$2.5 billion   
 Night Court                        	Night Court: Late hours.                                                                                               	               
 Oceanic Airlines                   	Lost                                                                                                                   	$7.8 billion   
 Omni Consumer Products             	Robocop: Making sure that the world is safe, with cutting edge robot-cop technology.                                   	               
 Oscorp                             	Spider-Man                                                                                                             	$3.1 billion   
 Paper Street Soap Company          	Fight Club: No direct deposit.                                                                                         	               
 Pendant Publishing                 	Seinfeld: Kick Piederman in the nads.                                                                                  	               
 Pied Piper                         	Silicon Valley: For when you want to work with the good guys.                                                          	               
 Pierce & Pierce                    	American Psycho: The best business cards in the business.                                                              	               
 Pizza Planet                       	Toy Story: If you’re a child at heart, this place is paradise.                                                         	               
 Powell Family                      	Charles in Charge: Professional babysitter.                                                                            	               
 Prestige Worldwide                 	Step Brother: You can never have too many black leather gloves.                                                        	               
 Pricemart                          	That 70’s Show: A “blast from the past” and a real groovy place to work.                                               	               
 Rekall                             	Inc., Total Recall: Quaid!                                                                                             	               
 Rex Kwan Do                        	Napoleon Dynamite: Vote for Jobacle!                                                                                   	               
 Rich Industries                    	Richie Rich                                                                                                            	$163.4 billion 
 Rodbell’s                          	Roseanne: Free apple pie.                                                                                              	               
 Silver Shamrock                    	Halloween III: Free masks (and snakes!)                                                                                	               
 Sirius Cybernetics Corp.           	Hitchhiker’s Guide                                                                                                     	$327.2 billion 
 Soylent Corp.                      	Soylent Green                                                                                                          	$157.1 billion 
 Soylent Corporation                	Soylent Green: Think of the free samples!                                                                              	               
 Spacely Space Sprockets            	The Jetsons                                                                                                            	$1.3 billion   
 Stark Industries                   	Iron Man                                                                                                               	$20.3 billion  
 Stay Puft Corporation              	Ghostbusters: Marshmallow madness.                                                                                     	               
 Sterling Cooper Draper Pryce       	Mad Men: If you can handle your liquor, you’re hired.                                                                  	               
 TGS with Tracy Jordan              	30 Rock: How television is made is always exciting!                                                                    	               
 TelAmeriCorp                       	Workaholics: Something tells us we might not be getting a lot done if we worked here.                                  	               
 The Daily Bugle                    	Spiderman: It’s easy to write articles for photos that good.                                                           	               
 The Dharma Initiative              	Lost: Who are these people?                                                                                            	               
 The Everything Store               	I Heart Huckabees: Discounts on everything.                                                                            	               
 The Foundation                     	Knight Rider: Your office is a truck. Your cube is a car.                                                              	               
 The Korova Milkbar                 	A Clockwork Orange: Great scenery.                                                                                     	               
 The Leftorium                      	The Simpsons: Argh! I’m right-handed.                                                                                  	               
 The Max                            	Saved by the Bell: Always open, never crowded.                                                                         	               
 The Reef                           	SpongeBob SquarePants: Movies, popcorn, Goobers – oh my!                                                               	               
 The Rusty Anchor                   	Golden Girls: I’d work there for the ladies.                                                                           	               
 The Smash Club                     	Full House: Work with the coolest guy in the world, Uncle Jesse.                                                       	               
 Tyrell Corporation                 	Blade Runner: More human than human? Sign us up!                                                                       	               
 Umbrella Corp.                     	Resident Evil                                                                                                          	$22.6 billion  
 Vandelay Industries                	Seinfeld: Work as a latex salesman!                                                                                    	               
 Very Big Corp. of America          	Monty Python                                                                                                           	$146.6 billion 
 Virtucon                           	Austin Powers                                                                                                          	$24.9 billion  
 WKRP                               	WKRP in Cincinnati: Venus Flytrap foreva.                                                                              	               
 Wally World                        	National Lampoon’s Vacation: They’re always closed.                                                                    	               
 Warbucks Industries                	Lil’ Orphan Annie                                                                                                      	$61.5 billion  
 Wayne Enterprises                  	Batman                                                                                                                 	$31.3 billion  
 Wonka Industries                   	Charlie...Choc. Factory                                                                                                	$21.0 billion  
 Weyland-Yutani Corporation         	                                                                                                                       	               
 Wu-Tang Financial                  	Chappelle’s Show: High-yield finds!                                                                                    	               
 Yoyodyne Propulsion Sys.           	Crying of Lot 49                                                                                                       	$5.8 billion   
 d'Anconia Copper                   	Atlas Shrugged                                                                                                         	$5.0 billion   ")))




(defun get-random-name (&optional required-fields)
  "Get a random name in the format expected by db-assert. If you pass
required fields you will get a name containing ALL of those fields,
but only those fields."
  (setf *random-state* (make-random-state t))
  (let ((names (if required-fields
                   (remove-if-not (lambda (name)
                                    (every (lambda (field)
                                             (find field name))
                                           (remove :insured-name-if-different required-fields)))
                                  *sample-names*)
                   *sample-names*)))
    (let ((found (nth (random (length names))
                      names)))
      (if required-fields
          (loop for x in required-fields
                collect x
                collect (if (eql x :insured-name-if-different)
                            (nth (random (length *companies*)) *companies*)
                            (second (member x found))))
          found))))


;; (get-random-name)

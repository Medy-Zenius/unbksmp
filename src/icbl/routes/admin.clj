(ns icbl.routes.admin
  (:require [compojure.core :refer :all]
            [icbl.views.layout :as layout]
            [noir.response :as resp]
            [noir.io :as io]
            [icbl.models.db :as db]
            [noir.session :as session]
            [clojure.data.json :as json]
            ))

(defn num-to-str [number]
  (let [snum (str number)]
    (cond
      (<= (count snum) 2) (str snum ",00")
      (= (subs snum 1 2) ".") (if (= (count (subs snum 2 (count snum))) 1) (str (subs snum 0 1) "," (subs snum 2 3) "0")
                                  (str (subs snum 0 1) "," (subs snum 2 (count snum))))
      )))

(defn admin-home []
  (layout/render "admin/home.html")
  )

(defn handle-login [pass]
  (let [vpass (:pass (db/get-data (str "select pass from admin where id='admin'") 1))]
    (if (= vpass pass)
        (do
          (session/put! :id "admin")
          (layout/render "admin/work.html"))
        (layout/render "admin/home.html" {:error "Password Salah!"}))))

(defn handle-ganti-pw-admin [pwlama pwbaru pwbaru1]
  (let [pwnow (:pass (db/get-data (str "select pass from admin where id='admin'") 1))]
    (if (or (not= pwlama pwnow) (< (count pwbaru) 5))
        (layout/render "admin/pesan.html" {:pesan "Password Lama tidak benar atau password baru kurang dari lima huruf!"})
        (if (= pwbaru pwbaru1)
          (try (db/update-data-1 "admin" ["id=?" "admin"] {:pass pwbaru})
                 (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah password admin!"})
               (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data password admin!"})))
          (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data password admin!"})))))

(defn handle-status-paket []
  (let [data (db/get-data (str "select * from paket order by kode asc") 2)]
    (layout/render "admin/status-paket.html" {:data data})))

(defn handle-ubah-status [kode]
  (let [data (db/get-data (str "select * from paket where kodesoal='" kode "'") 1)]
    (layout/render "admin/ubah-status.html" {:data data})
    ))
;;; Buat masukin kunci ke database
(defn handle-update-status1 [kodesoal status]
  (let [ kode (:kode (db/get-data (str "select kode from paket where kodesoal='" kodesoal "'") 1))
         kunci (clojure.string/replace (slurp (str "data/kunci/" kode ".rhs")) #"\r\n" "")]
  (try (db/update-data-1 "paket"
                              ["kodesoal=?" kodesoal]
                                      {:status status :kunci kunci})
               (handle-status-paket)
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan "Gagal mengubah status paket !"})))))

(defn handle-update-status [kodesoal status]
  (try (db/update-data-1 "paket"
                              ["kodesoal=?" kodesoal]
                                      {:status status})
               (handle-status-paket)
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan "Gagal mengubah status paket !"}))))

(defn hasil-to [act]
  (let [data (db/get-data (str "select kodesoal,pelajaran,keterangan from paket order by kode asc") 2)]
    (layout/render "admin/pilih-paket.html" {:data data :action act})))

(defn handle-hasil-to [kodesoal html]
  (let [mdata (db/get-data (str "select kodesoal,kode,pelajaran,keterangan,jsoal,kunci from paket where kodesoal='" kodesoal "'") 1)
        data (db/get-data (str "select datato.nis as nis,nama,nilai,jawaban from datato INNER JOIN paket ON paket.kode=datato.kode
                               INNER JOIN users ON users.nis=datato.nis
                               where paket.kodesoal='" kodesoal "' order by nilai desc") 2)
        ;data1 (map #(num-to-str (:nilai %)) data)
        ;kunci (clojure.string/replace (slurp (str "data/kunci/" (mdata :kode) ".rhs")) #"\r\n" "")
        kunci (mdata :kunci)
        ]
    (layout/render html {:data data :mdata mdata :kunci kunci})))

(defn abs [act]
  (let [data (db/get-data (str "select kodesoal,pelajaran,keterangan from paket order by kode asc") 2)]
    (layout/render "admin/pilih-paket.html" {:data data :action act})))

(defn hitung-bsk [no kun dt]
  (loop [[k b s] [0 0 0], j 0]
       (if (= j (count dt))
           [k b s]
           (recur
             (cond
                (= (subs (:jw (nth dt j)) no (inc no)) "-") [(inc k) b s]
                (= (subs (:jw (nth dt j)) no (inc no)) kun) [k (inc b) s]
                :else [k b (inc s)])
             (inc j))
         )))

(defn handle-abs [kodesoal html]
  (let [pkt (db/get-data (str "select kode,pelajaran,keterangan,kunci from paket where kodesoal='" kodesoal "'") 1)
        data (db/get-data (str "select jawaban as jw from datato where kode='" (pkt :kode) "'") 2)
        ;kunci (clojure.string/replace (slurp (str "data/kunci/" (pkt :kode) ".rhs")) #"\r\n" "")
        kunci (pkt :kunci)
        jsoal (count kunci)
        vhasil (loop [hsl [], i 0]
                     (if (= i jsoal)
                         hsl
                         (let [v (hitung-bsk i (subs kunci i (inc i)) data)] (recur (conj hsl v) (inc i)))))
        ]
        (layout/render html {:pelajaran (pkt :pelajaran)
                             :paket (pkt :keterangan)
                             :kodesoal kodesoal
                             :kode (pkt :kode)
                             :peserta (count data)
                             :hasil vhasil})))

(defn handle-abs-tk [kodesoal html]
  (let [pkt (db/get-data (str "select kode,pelajaran,keterangan,kunci from paket where kodesoal='" kodesoal "'") 1)
        datatk (db/get-data (str "select jawaban as jwtk,nilai from datato where kode='" (pkt :kode) "' order by nilai desc") 2)
        ;kunci (clojure.string/replace (slurp (str "data/kunci/" (pkt :kode) ".rhs")) #"\r\n" "")
        kunci (pkt :kunci)
        jsoal (count kunci)

        jdatatk (count datatk)
        jU (Math/round (* jdatatk 0.25))
        datatk1 (map #(% :jwtk) (concat (take jU datatk) (drop (- jdatatk jU) datatk)))
        cdatatk1 (count datatk1)
        vtk (loop [vt [] i 0]
              (if (= i jsoal)
                  vt
                  (let [kun (subs kunci i (inc i))
                        jbi (count
                              (filter (fn [x] (= x true))
                                (map #(= kun (subs % i (inc i))) datatk1)))
                        tk (/ (Math/round (/ jbi cdatatk1 0.01)) 100.0)] (recur (conj vt tk) (inc i)))))

        ;;;
        ]
      (layout/render html {:pelajaran (pkt :pelajaran)
                           :paket (pkt :keterangan)
                           :kodesoal kodesoal
                           :kode (pkt :kode)
                           :peserta (count datatk)
                           :hasil vtk})))

(defn handle-abs-dp [kodesoal html]
  (let [pkt (db/get-data (str "select kode,pelajaran,keterangan,kunci from paket where kodesoal='" kodesoal "'") 1)
        datatk (db/get-data (str "select jawaban as jwtk,nilai from datato where kode='" (pkt :kode) "' order by nilai desc") 2)
        ;kunci (clojure.string/replace (slurp (str "data/kunci/" (pkt :kode) ".rhs")) #"\r\n" "")
        kunci (pkt :kunci)
        jsoal (count kunci)

        jdatatk (count datatk)
        jU (Math/round (* jdatatk 0.25))
        datatkU (map #(% :jwtk) (take jU datatk))
        datatkL (map #(% :jwtk) (drop (- jdatatk jU) datatk))

        vdp (loop [dp [] i 0]
              (if (= i (count kunci))
                  dp
                  (let [kun (subs kunci i (inc i))
                        jbiU (count
                               (filter (fn [x] (= x true))
                                 (map #(= kun (subs % i (inc i))) datatkU)))
                        jbiL (count
                               (filter (fn [x] (= x true))
                                 (map #(= kun (subs % i (inc i))) datatkL)))
                        tdp (/ (Math/round (/ (- jbiU jbiL) jU 0.01)) 100.0)] (recur (conj dp tdp) (inc i)))))

        ;;;
        ]
      (layout/render html {:pelajaran (pkt :pelajaran)
                           :paket (pkt :keterangan)
                           :kodesoal kodesoal
                           :kode (pkt :kode)
                           :peserta (count datatk)
                           :hasil vdp})))

(defn hitung-abc [no kun dt]
  (loop [[a b c d k] [0 0 0 0 0], j 0]
       (if (= j (count dt))
           [kun a b c d k]
           (recur
             (cond
                (= (subs (:jw (nth dt j)) no (inc no)) "-") [a b c d (inc k)]
                (= (subs (:jw (nth dt j)) no (inc no)) "A") [(inc a) b c d k]
                (= (subs (:jw (nth dt j)) no (inc no)) "B") [a (inc b) c d k]
                (= (subs (:jw (nth dt j)) no (inc no)) "C") [a b (inc c) d k]
                (= (subs (:jw (nth dt j)) no (inc no)) "D") [a b c (inc d) k]
              )
             (inc j))
         )))

(defn handle-dayakecoh [kodesoal html]
  (let [pkt (db/get-data (str "select kode,pelajaran,keterangan,kunci from paket where kodesoal='" kodesoal "'") 1)
        data (db/get-data (str "select jawaban as jw from datato where kode='" (pkt :kode) "'") 2)
        kunci (pkt :kunci)
        ;kunci (clojure.string/replace (slurp (str "data/kunci/" (pkt :kode) ".rhs")) #"\r\n" "")
        jsoal (count kunci)
        vhasil (loop [hsl [], i 0]
                     (if (= i jsoal)
                         hsl
                         (let [v (hitung-abc i (subs kunci i (inc i)) data)] (recur (conj hsl v) (inc i)))))
        ]
        (layout/render html {:pelajaran (pkt :pelajaran)
                             :paket (pkt :keterangan)
                             :kodesoal kodesoal
                             :kode (pkt :kode)
                             :peserta (count data)
                             :hasil vhasil})))

(defn handle-to-detail-siswa [nis kodesoal]
  (let [pkt (db/get-data (str "select kode,pelajaran,keterangan,jsoal,kunci from paket where kodesoal='" kodesoal "'") 1)
        ;kunci (clojure.string/replace (slurp (str "data/kunci/" (pkt :kode) ".rhs")) #"\r\n" "")
        kunci (pkt :kunci)
        data (db/get-data (str "select datato.nis as nis,kode,jawaban,nilai,nama from datato
                               INNER JOIN users ON datato.nis=users.nis
                               where kode='" (pkt :kode) "' and datato.nis='" nis "'") 1)
        jawaban (data :jawaban)
        jsoal (pkt :jsoal)
        benar (count (filter true? (map #(= %1 %2) (vec kunci) (vec jawaban))))
        kosong (count (filter true? (map #(= % \-) (vec jawaban))))
        salah (- jsoal (+ benar kosong))
         ]
        (layout/render "admin/nilai-detail-siswa.html"
                       {:data data
                        :pelajaran (pkt :pelajaran)
                        :keterangan (pkt :keterangan)
                        :kodesoal kodesoal
                        :benar benar
                        :salah salah
                        :kosong kosong
                        :kunci kunci
                        :jawaban jawaban
                        :kode (pkt :kode)
                        })))

(defn rekap [act]
  (let [kdsoals (db/get-data (str "select kodesoal,kode from paket") 2)
        kdmat (sort-by :kodesoal (filter #(= (subs (% :kodesoal) 0 1) "1") kdsoals))
        kdipa (sort-by :kodesoal (filter #(= (subs (% :kodesoal) 0 1) "2") kdsoals))
        kdind (sort-by :kodesoal (filter #(= (subs (% :kodesoal) 0 1) "3") kdsoals))
        kding (sort-by :kodesoal (filter #(= (subs (% :kodesoal) 0 1) "4") kdsoals))
        ]
       (layout/render "admin/pilih-kdsoal.html" {:kdmat kdmat :kdipa kdipa :kdind kdind :kding kding :action act})
    ))


(defn mv-rekap [nistot dt mat ipa ind ing]
  [(nistot :nom)
   (nistot :nama)
   (let [nl (filter #(and (= (% :nis) (nistot :nom)) (= (% :kode) mat)) dt)]
     (if (not= nl []) ((nth nl 0) :nilai) "-"))
   (let [nl (filter #(and (= (% :nis) (nistot :nom)) (= (% :kode) ipa)) dt)]
     (if (not= nl []) ((nth nl 0) :nilai) "-"))
   (let [nl (filter #(and (= (% :nis) (nistot :nom)) (= (% :kode) ind)) dt)]
     (if (not= nl []) ((nth nl 0) :nilai) "-"))
   (let [nl (filter #(and (= (% :nis) (nistot :nom)) (= (% :kode) ing)) dt)]
     (if (not= nl []) ((nth nl 0) :nilai) "-"))
   (nistot :total)]
  )

(defn handle-rekap-set-to [mat ipa ind ing html]
  (let [nistot (db/get-data (str "select foo.nis as nom,total,nama from
                                 (select nis,sum(nilai) as total from datato where kode='" mat "' OR kode='" ipa "' OR
                                 kode='" ind "' OR kode='" ing "' group by nis) as foo INNER JOIN users
                                 ON foo.nis=users.nis order by nom") 2)
        cnt (count nistot)
        data (db/get-data (str "select nis,kode,nilai from datato") 2)
        vrekap (loop [vrek [] i 0]
                  (if (= i cnt)
                    vrek
                    (let [v (mv-rekap (nth nistot i) data mat ipa ind ing)] (recur (conj vrek v) (inc i)))))
        kmat (:kodesoal (db/get-data (str "select kodesoal from paket where kode='" mat "'") 1))
        kipa (:kodesoal (db/get-data (str "select kodesoal from paket where kode='" ipa "'") 1))
        kind (:kodesoal (db/get-data (str "select kodesoal from paket where kode='" ind "'") 1))
        king (:kodesoal (db/get-data (str "select kodesoal from paket where kode='" ing "'") 1))
        ]
    (layout/render html {:rekap vrekap
                         :kmat kmat :kipa kipa :kind kind :king king})))


(defn handle-list-nama [nm]
  (let [upnm (clojure.string/upper-case nm)
        data (db/get-data (str "select nis,nama,kelas from users where upper(nama) LIKE '%" upnm "%' order by nama") 2)]
       (if data
         (layout/render "admin/list-siswa-nama.html" {:data data})
         (layout/render "admin/pesan.html" {:pesan "Tidak ada nama tersebut!"}))
    ))

(defn handle-do-edit-siswa [nis]
  (layout/render "admin/edit-data-siswa.html"
                 {:datum (db/get-data (str "select * from users where nis='" nis "'") 1)}))

(defn handle-update-data-siswa [nislama nisbaru nama kelas email pass]
  (try (db/update-data-1 "users"
                              ["nis=?" nislama]
                                      {:nis nisbaru
                                       :nama nama
                                       :kelas kelas
                                       :email email
                                       :password pass})
               (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah data siswa!"})
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data siswa!"}))))

(defn lihat-guru []
  (let [data (db/get-data (str "select * from teacher order by nama asc") 2)]
    (layout/render "admin/lihat-guru.html" {:data data})))

(defn handle-edit-guru [id]
  (let [datum (db/get-data (str "select * from teacher where id='" id "'") 1)]
    (layout/render "admin/edit-guru.html" {:datum datum})))

(defn handle-update-guru [id nama pass]
  (try
    (db/update-data "teacher" (str "id='" id "'")
       {:nama nama
        :pass pass
        })
    (layout/render "admin/pesan.html" {:pesan (str "Berhasil Update Data Guru!")})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Update Data Guru error: " ex)}))))

(defn daftarkan-guru []
  (layout/render "admin/daftarkan-guru.html"))

(defn handle-daftarkan-guru [id nama]
  (do
    (io/create-path (str "resources/public/proset/" id) true)
    (try
      (db/insert-data "teacher" {:nama nama :id id :pass "abcde"})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil daftarkan Bapak/Ibu " nama " dengan ID " id)})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Daftarkan Guru error: " ex)})))))

(defn logout []
  (do
   (session/clear!)
   (resp/redirect "/admin")))

;;;routes
(defroutes admin-routes

  (GET "/admin" []
      (admin-home))

  (GET "/admin-home" []
       (layout/render "admin/work.html"))

  (GET "/admin-logout" []
       (logout))

  (POST "/admin-login" [pass]
      (handle-login pass))

  (GET "/status-paket" []
       (handle-status-paket))

  (POST "/ubah-status" [kodesoal]
       (handle-ubah-status kodesoal))

  (POST "/update-status" [kodesoal status]
        ;(println kodesoal)
       (handle-update-status kodesoal status))

  (GET "/hasil-to" []
       (hasil-to "/hasil-to"))
  (POST "/hasil-to" [kodesoal]
       (handle-hasil-to kodesoal "admin/hasil-to.html"))

  (GET "/hasil-to-excel" []
       (hasil-to "/hasil-to-excel"))
  (POST "/hasil-to-excel" [kodesoal]
       (handle-hasil-to kodesoal "admin/hasil-to-excel.html"))

  (GET "/abs" []
       (abs "/abs"))
  (POST "/abs" [kodesoal]
        (handle-abs kodesoal "admin/hasil-abs.html"))
  (GET "/abs-tk" []
       (abs "/abs-tk"))
  (POST "/abs-tk" [kodesoal]
        (handle-abs-tk kodesoal "admin/hasil-abs-tk.html"))
  (GET "/abs-dp" []
       (abs "/abs-dp"))
  (POST "/abs-dp" [kodesoal]
        (handle-abs-dp kodesoal "admin/hasil-abs-dp.html"))


  (GET "/abs-excel" []
       (abs "/abs-excel"))
  (POST "/abs-excel" [kodesoal]
        (handle-abs kodesoal "admin/hasil-abs-excel.html"))
  (GET "/abs-tk-excel" []
       (abs "/abs-tk-excel"))
  (POST "/abs-tk-excel" [kodesoal]
        (handle-abs-tk kodesoal "admin/hasil-abs-tk-excel.html"))
  (GET "/abs-dp-excel" []
       (abs "/abs-dp-excel"))
  (POST "/abs-dp-excel" [kodesoal]
        (handle-abs-dp kodesoal "admin/hasil-abs-dp-excel.html"))

  (GET "/dayakecoh" []
       (abs "/dayakecoh"))
  (POST "/dayakecoh" [kodesoal]
        (handle-dayakecoh kodesoal "admin/hasil-dayakecoh.html"))

  (GET "/adk-excel" []
       (abs "/adk-excel"))
  (POST "/adk-excel" [kodesoal]
        (handle-dayakecoh kodesoal "admin/hasil-adk-excel.html"))

  (POST "/to-detail-siswa" [nis kodesoal]
        (handle-to-detail-siswa nis kodesoal))

  (POST "/lihat-soal" [nomer kode]
        (layout/render "admin/lihat-soal.html" {:nomer nomer :kode kode}))

  (GET "/rekap" []
       (rekap "/rekap-set-to"))
  (POST "/rekap-set-to" [mat ipa ind ing]
        (handle-rekap-set-to mat ipa ind ing "admin/list-rekap-to.html"))

  (GET "/rekap-excel" []
       (rekap "/rekap-set-to-excel"))
  (POST "/rekap-set-to-excel" [mat ipa ind ing]
        (handle-rekap-set-to mat ipa ind ing "admin/list-rekap-to-excel.html"))

  (GET "/edit-siswa" []
       (layout/render "admin/search-siswa.html"))
  (POST "/edit-siswa" [nama]
        (handle-list-nama nama))
  (POST "/do-edit-siswa" [nis]
        (handle-do-edit-siswa nis))
  (POST "/update-data-siswa" [nislama nisbaru nama kelas email pass]
        (handle-update-data-siswa nislama nisbaru nama kelas email pass))

  (GET "/ganti-pw-admin" []
       (layout/render "admin/ganti-pw-admin.html"))
  (POST "/ganti-pw-admin" [pwlama pwbaru pwbaru1]
        (handle-ganti-pw-admin pwlama pwbaru pwbaru1))

  (GET "/lihat-guru" []
       (lihat-guru))
  (POST "/edit-guru" [id]
        (handle-edit-guru id))
  (POST "/update-guru" [id nama pass]
        (handle-update-guru id nama pass))

  (GET "/daftarkan-guru" []
       (daftarkan-guru))
  (POST "/daftarkan-guru" [id nama]
        (handle-daftarkan-guru id nama))

)

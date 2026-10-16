-- Convenience views over schema.sql.
-- Statements are separated by a line holding only ";;".

-- fits + method family, with the parameters the readers filter on pivoted out
-- of fit_params.
CREATE VIEW v_fits AS
SELECT f.*,
       m.family,
       (SELECT num_value FROM fit_params p WHERE p.fit_id = f.fit_id AND p.name = 'para')  AS para,
       (SELECT num_value FROM fit_params p WHERE p.fit_id = f.fit_id AND p.name = 'power') AS power
FROM fits f
JOIN methods m ON m.method = f.method
;;
-- Every match in both orientations (this fit/factor -> the other), with
-- each metric's score, so per-fit and per-factor queries need no UNION.
CREATE VIEW v_factor_matches AS
SELECT fp.fit_pair_id, fm.match_id, fa.dataset_id, fa.method, fp.same_rank,
       fp.fit_a AS fit_id, fa.rank AS rank, fa.seed AS seed, xa.factor_id AS factor_id,
       xa.factor_index AS factor_index,
       fp.fit_b AS other_fit_id, fb.rank AS other_rank, fb.seed AS other_seed,
       xb.factor_id AS other_factor_id, xb.factor_index AS other_factor_index,
       ms.metric, ms.value, ms.runner_up, ms.margin
FROM factor_matches fm
JOIN fit_pairs fp ON fp.fit_pair_id = fm.fit_pair_id
JOIN match_scores ms ON ms.match_id = fm.match_id
JOIN factors xa ON xa.factor_id = fm.factor_a
JOIN factors xb ON xb.factor_id = fm.factor_b
JOIN fits fa ON fa.fit_id = fp.fit_a
JOIN fits fb ON fb.fit_id = fp.fit_b
UNION ALL
SELECT fp.fit_pair_id, fm.match_id, fb.dataset_id, fb.method, fp.same_rank,
       fp.fit_b, fb.rank, fb.seed, xb.factor_id, xb.factor_index,
       fp.fit_a, fa.rank, fa.seed, xa.factor_id, xa.factor_index,
       ms.metric, ms.value, ms.runner_up, ms.margin
FROM factor_matches fm
JOIN fit_pairs fp ON fp.fit_pair_id = fm.fit_pair_id
JOIN match_scores ms ON ms.match_id = fm.match_id
JOIN factors xa ON xa.factor_id = fm.factor_a
JOIN factors xb ON xb.factor_id = fm.factor_b
JOIN fits fa ON fa.fit_id = fp.fit_a
JOIN fits fb ON fb.fit_id = fp.fit_b
;;
-- What the DB holds per (dataset, method); replaces the old `ingests` table.
CREATE VIEW v_ingest_summary AS
SELECT f.dataset_id, f.method, m.family,
       COUNT(*)                     AS n_fits,
       SUM(f.status = 'ok')         AS n_ok,
       SUM(f.status = 'failed')     AS n_failed,
       SUM(f.representative)        AS n_representative
FROM fits f
JOIN methods m ON m.method = f.method
GROUP BY f.dataset_id, f.method
;;
-- Matched |similarity| of every factor across the same-rank fit pairs of a
-- method, per metric; kind says whether a bootstrap fit is involved.
CREATE VIEW v_same_rank_matches AS
SELECT fa.dataset_id, fa.method, fa.rank, p.fit_pair_id, s.metric, ABS(s.value) AS value,
       CASE WHEN COALESCE(fa.bootstrap, 0) + COALESCE(fb.bootstrap, 0) > 0
            THEN 'with bootstrap' ELSE 'seeds only' END AS kind
FROM fit_pairs p
JOIN fits fa ON fa.fit_id = p.fit_a
JOIN fits fb ON fb.fit_id = p.fit_b
JOIN factor_matches m ON m.fit_pair_id = p.fit_pair_id
JOIN match_scores s ON s.match_id = m.match_id
WHERE p.same_rank = 1
;;
-- Projections with the source fit's dataset, method, rank and seed.
CREATE VIEW v_projections AS
SELECT p.*, f.dataset_id AS source_dataset_id, f.method, f.rank, f.seed, f.representative
FROM projections p
JOIN fits f ON f.fit_id = p.source_fit_id

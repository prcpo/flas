--
-- FLAS - Free/libre accounting system
--
-- Load initial content
-- You need to create database first!
-- 
-- PostgreSQL version 9.1.1
-- Required ltree and plpgsql contrib modules
-- 

insert into app.modes (tree, lbl) 
VALUES ('doc', 'Документы');

insert into app.modes (tree, lbl) 
VALUES ('dic', 'Справочники');

insert into app.modes (tree, lbl) 
VALUES ('doc.pay', 'Платёжные поручения');

insert into app.modes (tree, lbl) 
VALUES ('dic.companies', 'Организации');



insert into dic.param_def (tree, lbl, note) 
VALUES ('defaults', 'Настройки по-умолчанию', NULL::text);

insert into dic.param_def (tree, lbl, note) 
VALUES ('defaults.company_name', 'Наименование организации по-умолчанию', NULL::text);


insert into dic.param_def (tree, lbl, note) 
VALUES ('session','Параметры текущего сеанса', NULL::text);

insert into dic.param_def (tree, lbl, note) 
VALUES ('session.work_date', NULL::text, NULL::text);

insert into dic.param_def (tree, lbl, note) 
VALUES ('session.active_company', NULL::text, NULL::text);


insert into dic.param_values (usr, cmp, param, val) 
VALUES ( '', NULL, 'defaults.company_name', 'Моя организация');


insert into test.tests (code, res) 
VALUES ('param_value_get(''defaults.company_name'')', 'Моя организация');



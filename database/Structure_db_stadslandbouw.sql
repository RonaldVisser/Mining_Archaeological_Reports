--
-- PostgreSQL database dump
--

-- Dumped from database version 10.4
-- Dumped by pg_dump version 10.4

-- Started on 2018-07-10 22:33:36

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 1 (class 3079 OID 12924)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2813 (class 0 OID 0)
-- Dependencies: 1
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 198 (class 1259 OID 16419)
-- Name: tbl_docs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tbl_docs (
    filename text NOT NULL,
    path text,
    pages integer
);


ALTER TABLE public.tbl_docs OWNER TO postgres;

--
-- TOC entry 197 (class 1259 OID 16402)
-- Name: tbl_keywords; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tbl_keywords (
    keyword text NOT NULL,
    value integer,
    category text
);


ALTER TABLE public.tbl_keywords OWNER TO postgres;

--
-- TOC entry 196 (class 1259 OID 16394)
-- Name: tbl_term_docs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tbl_term_docs (
    word text NOT NULL,
    freq integer,
    filename text NOT NULL
);


ALTER TABLE public.tbl_term_docs OWNER TO postgres;

--
-- TOC entry 2680 (class 2606 OID 16401)
-- Name: tbl_term_docs pk_dtm; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tbl_term_docs
    ADD CONSTRAINT pk_dtm PRIMARY KEY (word, filename);


--
-- TOC entry 2684 (class 2606 OID 16426)
-- Name: tbl_docs tbl_docs_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tbl_docs
    ADD CONSTRAINT tbl_docs_pkey PRIMARY KEY (filename);


--
-- TOC entry 2682 (class 2606 OID 16409)
-- Name: tbl_keywords tbl_keywords_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tbl_keywords
    ADD CONSTRAINT tbl_keywords_pkey PRIMARY KEY (keyword);


-- Completed on 2018-07-10 22:33:36

--
-- PostgreSQL database dump complete
--


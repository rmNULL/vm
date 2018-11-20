PRAGMA foreign_keys=on;

CREATE TABLE IF NOT EXISTS People(
	id integer primary key,
	name text DEFAULT '',
	address text DEFAULT '',
	permanent boolean DEFAULT true,
	relation text NOT NULL,
	CHECK(relation IN ('customer', 'supplier', 'both'))
);

CREATE TABLE IF NOT EXISTS Contacts(
	person REFERENCES People(id) ON DELETE CASCADE,
	label text DEFAULT 'other', -- home/mobile/work/other
	number text PRIMARY KEY NOT NULL
);

CREATE TABLE IF NOT EXISTS BankAccounts(
	person REFERENCES People(id) ON DELETE CASCADE,
	name text default '',
	number text PRIMARY KEY,
	IFSC text,
	bank text,
	branch text
);


CREATE TABLE IF NOT EXISTS Inventory(
	lot integer PRIMARY KEY,
	date datetime,
	supplier REFERENCES People(id) ON DELETE CASCADE,
	status text NOT NULL DEFAULT 'open',
	CHECK(status in ('open', 'closed'))
);

CREATE TABLE IF NOT EXISTS Items(
	lot REFERENCES Inventory(lot),
	name text,
	qty integer,
	stock integer,
	ppu number, -- price per unit
	package text, -- bag / butti / ...
	package_count integer,
	PRIMARY KEY (lot, name)
);

CREATE TABLE IF NOT EXISTS Invoice(
	number integer PRIMARY KEY,
	bill_number integer, -- manually maintained entry
	date datetime,
	customer REFERENCES People(id) ON DELETE CASCADE,
	total number
);

CREATE TABLE IF NOT EXISTS Sold(
	invoice REFERENCES Invoice(number) ON UPDATE CASCADE,
	lot,
	item,
	qty integer,
	amount number, -- amount = ppu * qty
	package text,
	package_count integer,
	FOREIGN KEY (lot, item) REFERENCES Items(lot, name)
);

CREATE TABLE IF NOT EXISTS MoneyTransaction(
	ref_id integer PRIMARY KEY,
	person REFERENCES People(id) ON DELETE CASCADE,
	date datetime,
	credit number,
	debit number,
	total number,
	description text
);

CREATE view contact_labels
AS
 select DISTINCT(label) from Contacts
 union
 select 'mobile'
 union
 select 'work'
 union
 select 'other';

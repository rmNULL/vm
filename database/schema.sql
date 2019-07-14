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

CREATE TABLE IF NOT EXISTS Inventory(
	lot integer PRIMARY KEY,
	date datetime,
	supplier REFERENCES People(id) ON DELETE CASCADE,
	status text NOT NULL DEFAULT 'open',
	CHECK(status in ('open', 'closed'))
);

CREATE TABLE IF NOT EXISTS Items(
	lot REFERENCES Inventory(lot) ON DELETE CASCADE,
	name text,
	qty float,
	stock float,
	/* ppu number, -- price per unit */
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
	qty number,
	ppu number,
	amount number, -- amount = ppu * qty
	package text,
	package_count integer,
	FOREIGN KEY (lot, item) REFERENCES Items(lot, name)
);

CREATE TABLE IF NOT EXISTS SoldMisc(
	invoice REFERENCES Invoice(number) ON UPDATE CASCADE,
	description text,
	amount number
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

CREATE TABLE IF NOT EXISTS BankAccounts(
	person REFERENCES People(id) ON DELETE CASCADE,
	name text default '',
	number text PRIMARY KEY,
	IFSC text,
	bank text,
	branch text
);

CREATE TABLE IF NOT EXISTS InventoryOtherCosts(
       id integer PRIMARY KEY,
       lot references Inventory(lot) ON DELETE CASCADE,
       lorry_frieght number DEFAULT 0
);

CREATE VIEW IF NOT EXISTS contact_labels
AS
 select DISTINCT(label) from Contacts
 union
 select 'mobile'
 union
 select 'work'
 union
 select 'other';

CREATE TABLE IF NOT EXISTS OpeningBalance(
    open_date date PRIMARY KEY,
    amount number);
    
CREATE TABLE IF NOT EXISTS DayBook(
      id integer PRIMARY KEY,
      date datetime,
      description text,
      payment_method text,
      category text,
      credit NUMBER default 0,
      debit NUMBER default 0,
      Total NUMBER);

-- Although named as LedgerInvoice read it as InvoiceLedger,
--  the ref table is suffixed, to make it easier for code autocompletion, and
--  remembering of names :)

--  maintain discounts given after an invoice is issued
(LedgerInvoice . "CREATE TABLE IF NOT EXISTS LedgerInvoice(
	invoice REFERENCES Invoice(number) ON UPDATE CASCADE NOT NULL,
	description text,
	amount NUMBER default 0,
	PRIMARY KEY(invoice)
);")

-- 
(LedgerTrader . "CREATE TABLE IF NOT EXISTS LedgerTrader(
	id integer PRIMARY KEY,
	trader REFERENCES People(id) ON DELETE CASCADE,
	daybook_entry REFERENCES DayBook(id) ON UPDATE CASCADE NOT NULL,
	role text,
	CHECK( role IN ('customer', 'supplier') )
);")


-- 
(LedgerMisc . "CREATE TABLE IF NOT EXISTS LedgerMisc(
	id integer PRIMARY KEY,
	date datetime,
	trader REFERENCES People(id) ON DELETE CASCADE,
	role text,
	amount number,
	description text,
	CHECK( role IN ('customer', 'supplier') )
);")


/* CREATE VIEW IF NOT EXISTS Ledger */
/* AS */
/* ; */



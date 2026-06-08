-- Create the _latest_ tables for daily aggregation snapshots
-- Run these commands in the Neon console to set up the new tables

CREATE TABLE city_latest_summary (
    id SERIAL PRIMARY KEY,
    snapshot_date DATE,
    city VARCHAR(100),
    listing_type VARCHAR(10),      -- 'buy' or 'rent'
    listing_count INTEGER,
    median_price NUMERIC,
    median_price_per_m2 NUMERIC,
    avg_time_on_market_days NUMERIC,
    p25_price NUMERIC,
    p75_price NUMERIC,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE neighbourhood_latest_summary (
    id SERIAL PRIMARY KEY,
    snapshot_date DATE,
    city VARCHAR(100),
    neighbourhood VARCHAR(200),
    listing_type VARCHAR(10),
    listing_count INTEGER,
    median_price NUMERIC,
    median_price_per_m2 NUMERIC,
    avg_time_on_market_days NUMERIC,
    most_common_property_type VARCHAR(20),
    created_at TIMESTAMP DEFAULT NOW()
);

-- Create indexes for faster queries
CREATE INDEX idx_city_latest_city ON city_latest_summary(city);
CREATE INDEX idx_city_latest_type ON city_latest_summary(listing_type);
CREATE INDEX idx_neighbourhood_latest_city_nbhd ON neighbourhood_latest_summary(city, neighbourhood);
CREATE INDEX idx_neighbourhood_latest_type ON neighbourhood_latest_summary(listing_type);

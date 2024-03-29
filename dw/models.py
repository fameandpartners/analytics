from datetime import datetime
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column, Integer, Float, String, Date, DateTime, Boolean

Base = declarative_base()

def _get_time():
    return datetime.now()

class Sale(Base):
    __tablename__ = 'sales'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    line_item_id = Column(Float)
    order_id = Column(Float)
    order_number = Column(String)
    order_state = Column(String)
    payment_state = Column(String)
    completed_timestamp = Column(DateTime)
    total = Column(Float)
    item_total = Column(Float)
    email = Column(String)
    user_id = Column(Float)
    customer_name = Column(String)
    currency = Column(String)
    ship_address_id = Column(Float)
    quantity = Column(Float)
    price = Column(Float)
    product_id = Column(Float)
    v_height = Column(Float)
    g_size = Column(String)
    gross_extra_attributed = Column(Float)
    net_extra_attributed = Column(Float)
    conversion_rate = Column(Float)
    sales_usd = Column(Float)
    gross_revenue_usd = Column(Float)
    adjustments_total_percentage = Column(Float)
    adjustments_usd = Column(Float)
    order_date = Column(Date)
    customized = Column(Float)
    lip_size = Column(String)
    customisation_value_ids = Column(String)
    color = Column(String)
    lip_height = Column(String)
    style_number = Column(String)
    style_name = Column(String)
    ship_city = Column(String)
    ship_state = Column(String)
    ship_country = Column(String)
    li_ship_date = Column(Date)
    refund_amount = Column(Float)
    return_comments = Column(String)
    reason_category = Column(String)
    reason_sub_category = Column(String)
    acceptance_status = Column(String)
    factory_fault = Column(Boolean)
    factory_name = Column(String)
    order_payments = Column(Float)
    total_payment_amount = Column(Float)
    coupon_code = Column(String)
    length = Column(String)
    assigned_cohort = Column(String)
    correct_ship_date = Column(Date)
    making_option = Column(String)
    payments = Column(Float)
    item_total_usd = Column(Float)
    promotions_usd = Column(Float)
    shipping_usd = Column(Float)
    taxes_usd = Column(Float)
    other_adjustments_usd = Column(Float)
    ship_date = Column(Date)
    refund_amount_usd = Column(Float)
    price_usd = Column(Float)
    height = Column(String)
    size = Column(String)
    us_size_str = Column(String)
    au_size_str = Column(String)
    return_order_id = Column(Float)
    is_shipped = Column(Boolean)
    return_requested = Column(Boolean)
    item_returned = Column(Boolean)
    order_status = Column(String)
    return_reason = Column(String)
    estimated_ship_date = Column(Date)
    ship_year_month = Column(String)
    order_year_month = Column(String)
    payment_processing_cost = Column(Float)
    physically_customized = Column(Float)
    repeat_purchase = Column(Boolean)
    collection = Column(String)
    us_size = Column(Float)
    manufacturing_cost = Column(Float)
    ship_year = Column(Float)
    ship_month = Column(Float)
    avg_order_shipping_cost = Column(Float)
    li_shipping_cost = Column(Float)
    units_in_order = Column(Float)
    asset_id = Column(Float)
    attachment_file_name = Column(String)
    attachment_width = Column(Float)
    attachment_height = Column(Float)
    dress_image_url = Column(String)
    dress_image_tag = Column(String)

    def __repr__(self):
        return f'<Sale(item:{self.line_item_id} number:{self.order_number})>'

class Product(Base):
    __tablename__ = 'products'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    product_id = Column(Integer)
    style_number = Column(String)
    style_name = Column(String)
    factory_name = Column(String)
    available_on = Column(Date)
    live = Column(Boolean)

    def __repr__(self):
        return f'<Product(style_name:{self.style_name} style_number:{self.style_number})>'

class ProductTaxon(Base):
    __tablename__ = 'product_taxons'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    product_id = Column(Integer)
    taxon_name = Column(String)

    def __repr__(self):
        return f'<ProductTaxon {self.taxon_name}>'

class FacebookImage(Base):
    __tablename__ = 'facebook_images'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    ad_name = Column(String)
    ad_image = Column(String)

class CustomizationValue(Base):
    __tablename__ = 'customization_values'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    customization_value_id = Column(Integer)
    presentation = Column(String)
    price = Column(Float)

    def __repr__(self):
        return f'<CustomizationValue {self.presentation}>'

class LineItemCustomization(Base):
    __tablename__ = 'line_item_customizations'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    line_item_id = Column(Integer)
    customization_value_id = Column(Integer)

    def __repr__(self):
        return f'<LineItemCustomization {self.line_item_id}>'

class CohortAssignment(Base):
    __tablename__ = 'cohort_assignments'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    email = Column(String)
    cohort = Column(String)

    def __repr__(self):
        return f'<CohortAssignment {self.email} {self.cohort}>'

class DailyKPI(Base):
    __tablename__ = 'daily_kpis'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    date = Column(Date)
    gross_revenue = Column(Float)
    net_sales = Column(Float)
    orders = Column(Float)
    units = Column(Float)
    customized_units = Column(Float)
    refulfilled_units = Column(Float)
    cogs = Column(Float)
    packaging_materials = Column(Float)
    product_cost = Column(Float)
    discounts = Column(Float)
    shipping = Column(Float)
    taxes = Column(Float)
    other_adjustments = Column(Float)
    transactions = Column(Float)
    new_customers = Column(Float)
    repeat_customers = Column(Float)
    returns = Column(Float)
    inventory_returns = Column(Float)
    refulfilled_return_units = Column(Float)
    promoters = Column(Float)
    detractors = Column(Float)
    responses = Column(Float)

    def __repr__(self):
        date_str = str(self.date)
        return f'<DailyKPI {date_str}>'

class MonthlyCohortKPI(Base):
    __tablename__ = 'monthly_kpis'

    id = Column(Integer, primary_key=True)
    created_at = Column(DateTime, default=_get_time)
    a = Column(String)
    b = Column(String)
    c = Column(String)
    d = Column(String)
    value = Column(Float)
    year_month = Column(String)

    def __repr__(self):
        return f'<MonthlyCohortKPI {self.a} | {self.b} | {self.c} | {self.d} | {self.year_month}>'

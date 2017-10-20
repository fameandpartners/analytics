import os
from feather import read_dataframe
from sqlalchemy.orm import sessionmaker
from sqlalchemy.schema import CreateTable, DropTable
import pandas as pd
from models import (
    Sale, Product, ProductTaxon, FacebookImage, CustomizationValue,
    LineItemCustomization, DailyKPI, CohortAssignment
)
import reports
import warnings

# ignore numpy's annoying date warnings in convert_date
warnings.simplefilter("ignore", UserWarning)

__location__ = os.path.realpath(os.path.join(os.getcwd(), os.path.dirname(__file__)))
FEATHERS = os.path.join(__location__, 'feathers/')

def convert_date(date, type_of):
    if date != (None,) and not isinstance(date, pd._libs.tslib.NaTType) and date:
        if type_of == 'DateTime':
            return date.to_pydatetime()
        elif type_of == 'Date':
            return date.to_pydatetime().date()
    else:
        return None

def load_sales(engine):
    Session = sessionmaker(bind=engine)
    sales_df = read_dataframe(FEATHERS + 'sales.feather')
    sales_dicts = sales_df.to_dict(orient='record')
    sales = []
    for record in sales_dicts:
        sale = Sale(
            factory_fault = record.get('factory_fault'),
            is_shipped = record.get('is_shipped'),
            item_returned = record.get('item_returned'),
            repeat_purchase = record.get('repeat_purchase'),
            return_requested = record.get('return_requested'),
            correct_ship_date = convert_date(record.get('correct_ship_date'), 'Date'),
            estimated_ship_date = convert_date(record.get('estimated_ship_date'), 'Date'),
            li_ship_date = convert_date(record.get('li_ship_date'), 'Date'),
            order_date = convert_date(record.get('order_date'), 'Date'),
            ship_date = convert_date(record.get('ship_date'), 'Date'),
            completed_timestamp = convert_date(record.get('completed_timestamp'), 'DateTime'),
            adjustments_total_percentage = record.get('adjustments_total_percentage'),
            adjustments_usd = record.get('adjustments_usd'),
            avg_order_shipping_cost = record.get('avg_order_shipping_cost'),
            conversion_rate = record.get('conversion_rate'),
            gross_extra_attributed = record.get('gross_extra_attributed'),
            gross_revenue_usd = record.get('gross_revenue_usd'),
            item_total = record.get('item_total'),
            item_total_usd = record.get('item_total_usd'),
            li_shipping_cost = record.get('li_shipping_cost'),
            manufacturing_cost = record.get('manufacturing_cost'),
            net_extra_attributed = record.get('net_extra_attributed'),
            other_adjustments_usd = record.get('other_adjustments_usd'),
            payment_processing_cost = record.get('payment_processing_cost'),
            payments = record.get('payments'),
            physically_customized = record.get('physically_customized'),
            price = record.get('price'),
            price_usd = record.get('price_usd'),
            promotions_usd = record.get('promotions_usd'),
            refund_amount_usd = record.get('refund_amount_usd'),
            sales_usd = record.get('sales_usd'),
            shipping_usd = record.get('shipping_usd'),
            taxes_usd = record.get('taxes_usd'),
            total = record.get('total'),
            total_payment_amount = record.get('total_payment_amount'),
            v_height = record.get('v_height'),
            asset_id = record.get('asset_id'),
            attachment_height = record.get('attachment_height'),
            attachment_width = record.get('attachment_width'),
            customized = record.get('customized'),
            line_item_id = record.get('line_item_id'),
            order_id = record.get('order_id'),
            order_payments = record.get('order_payments'),
            product_id = record.get('product_id'),
            quantity = record.get('quantity'),
            refund_amount = record.get('refund_amount'),
            return_order_id = record.get('return_order_id'),
            ship_address_id = record.get('ship_address_id'),
            ship_month = record.get('ship_month'),
            ship_year = record.get('ship_year'),
            units_in_order = record.get('units_in_order'),
            us_size = record.get('us_size'),
            user_id = record.get('user_id'),
            acceptance_status = record.get('acceptance_status'),
            assigned_cohort = record.get('assigned_cohort'),
            attachment_file_name = record.get('attachment_file_name'),
            au_size_str = record.get('au_size_str'),
            collection = record.get('collection'),
            color = record.get('color'),
            coupon_code = record.get('coupon_code'),
            currency = record.get('currency'),
            customer_name = record.get('customer_name'),
            customisation_value_ids = record.get('customisation_value_ids'),
            dress_image_tag = record.get('dress_image_tag'),
            dress_image_url = record.get('dress_image_url'),
            email = record.get('email'),
            factory_name = record.get('factory_name'),
            g_size = record.get('g_size'),
            height = record.get('height'),
            length = record.get('length'),
            lip_height = record.get('lip_height'),
            lip_size = record.get('lip_size'),
            making_option = record.get('making_option'),
            order_number = record.get('order_number'),
            order_state = record.get('order_state'),
            order_status = record.get('order_status'),
            order_year_month = record.get('order_year_month'),
            payment_state = record.get('payment_state'),
            reason_category = record.get('reason_category'),
            reason_sub_category = record.get('reason_sub_category'),
            return_comments = record.get('return_comments'),
            return_reason = record.get('return_reason'),
            ship_city = record.get('ship_city'),
            ship_country = record.get('ship_country'),
            ship_state = record.get('ship_state'),
            ship_year_month = record.get('ship_year_month'),
            size = record.get('size'),
            style_name = record.get('style_name'),
            style_number = record.get('style_number'),
            us_size_str = record.get('us_size_str'),
        )
        sales.append(sale)
    session = Session()
    if Sale.__tablename__ in engine.table_names():
        session.execute(DropTable(Sale.__table__))
    session.execute(CreateTable(Sale.__table__))
    session.add_all(sales)
    session.commit()
    session.close()

def load_products(engine):
    Session = sessionmaker(bind=engine)
    products_df = read_dataframe(FEATHERS + 'products.feather')
    products_dicts = products_df.to_dict(orient='record')
    products = []
    for record in products_dicts:
        product = Product(
            product_id = record.get('product_id'),
            style_number = record.get('style_number'),
            style_name = record.get('style_name'),
            factory_name = record.get('factory_name'),
            available_on = convert_date(record.get('available_on'), 'Date'),
            live = record.get('live')
        )
        products.append(product)
    session = Session()
    if Product.__tablename__ in engine.table_names():
        session.execute(DropTable(Product.__table__))
    session.execute(CreateTable(Product.__table__))
    session.add_all(products)
    session.commit()
    session.close()

def load_product_taxons(engine):
    Session = sessionmaker(bind=engine)
    product_taxons_df = read_dataframe(FEATHERS + 'product_taxons.feather')
    product_taxons_dicts = product_taxons_df.to_dict(orient='record')
    product_taxons = []
    for record in product_taxons_dicts:
        product_taxon = ProductTaxon(
            product_id = record.get('product_id'),
            taxon_name = record.get('taxon_name')
        )
        product_taxons.append(product_taxon)
    session = Session()
    if ProductTaxon.__tablename__ in engine.table_names():
        session.execute(DropTable(ProductTaxon.__table__))
    session.execute(CreateTable(ProductTaxon.__table__))
    session.add_all(product_taxons)
    session.commit()
    session.close()

def load_facebook_images(engine):
    Session = sessionmaker(bind=engine)
    facebook_images_df = read_dataframe(FEATHERS + 'facebook_images.feather')
    facebook_images_dicts = facebook_images_df.to_dict(orient='record')
    facebook_images = []
    for record in facebook_images_dicts:
        facebook_image = FacebookImage(
            ad_name = record.get('ad_name'),
            ad_image = record.get('ad_image')
        )
        facebook_images.append(facebook_image)
    session = Session()
    if FacebookImage.__tablename__ in engine.table_names():
        session.execute(DropTable(FacebookImage.__table__))
    session.execute(CreateTable(FacebookImage.__table__))
    session.add_all(facebook_images)
    session.commit()
    session.close()

def load_customization_values(engine):
    Session = sessionmaker(bind=engine)
    customization_values_df = read_dataframe(FEATHERS + 'customization_values.feather')
    customization_values_dicts = customization_values_df.to_dict(orient='record')
    customization_values = []
    for record in customization_values_dicts:
        customization_value = CustomizationValue(
            customization_value_id = record.get('customization_value_id'),
            presentation = record.get('presentation'),
            price = record.get('price')
        )
        customization_values.append(customization_value)
    session = Session()
    if CustomizationValue.__tablename__ in engine.table_names():
        session.execute(DropTable(CustomizationValue.__table__))
    session.execute(CreateTable(CustomizationValue.__table__))
    session.add_all(customization_values)
    session.commit()
    session.close()

def load_line_item_customizations(engine):
    Session = sessionmaker(bind=engine)
    line_item_customizations_df = read_dataframe(FEATHERS + 'line_item_customizations.feather')
    line_item_customizations_dicts = line_item_customizations_df.to_dict(orient='record')
    line_item_customizations = []
    for record in line_item_customizations_dicts:
        line_item_customization = LineItemCustomization(
            line_item_id = int(record.get('line_item_id')),
            customization_value_id = int(record.get('customization_value_id'))
        )
        line_item_customizations.append(line_item_customization)
    session = Session()
    if LineItemCustomization.__tablename__ in engine.table_names():
        session.execute(DropTable(LineItemCustomization.__table__))
    session.execute(CreateTable(LineItemCustomization.__table__))
    session.add_all(line_item_customizations)
    session.commit()
    session.close()

def load_daily_kpis(engine):
    Session = sessionmaker(bind=engine)
    daily_kpis_df = reports.daily_kpis()
    daily_kpis_dicts = daily_kpis_df.to_dict(orient='record')
    daily_kpis = []
    for record in daily_kpis_dicts:
        daily_kpi = DailyKPI(
            date = convert_date(record.get('Date'), 'DateTime'),
            gross_revenue = record.get('Gross Revenue'),
            net_sales = record.get('Net Sales'),
            orders = record.get('Orders'),
            units = record.get('Units'),
            customized_units = record.get('Customized Units'),
            refulfilled_units = record.get('Re-fulfilled Units'),
            cogs = record.get('COGS'),
            packaging_materials = record.get('Packaging Materials'),
            product_cost = record.get('Product Cost'),
            discounts = record.get('Discounts'),
            shipping = record.get('Shipping'),
            taxes = record.get('Taxes'),
            other_adjustments = record.get('Other Adjustments'),
            transactions = record.get('Transactions'),
            new_customers = record.get('New Customers'),
            repeat_customers = record.get('Repeat Customers'),
            returns = record.get('Returns'),
            inventory_returns = record.get('Inventory Returns'),
            refulfilled_return_units = record.get('Refulfilled Return Units'),
            promoters = record.get('Promoters'),
            detractors = record.get('Detractors'),
            responses = record.get('Responses')
        )
        daily_kpis.append(daily_kpi)
    session = Session()
    if DailyKPI.__tablename__ in engine.table_names():
        session.execute(DropTable(DailyKPI.__table__))
    session.execute(CreateTable(DailyKPI.__table__))
    session.add_all(daily_kpis)
    session.commit()
    session.close()

def load_cohort_assignments(engine):
    Session = sessionmaker(bind=engine)
    cohort_assignments_df = read_dataframe(FEATHERS + 'cohort_assignments.feather')
    cohort_assignments_dicts = cohort_assignments_df.to_dict(orient='record')
    cohort_assignments = []
    for record in cohort_assignments_dicts:
        cohort_assignment = CohortAssignment(
            email = record.get('email'),
            cohort = record.get('assigned_cohort')
        )
        cohort_assignments.append(cohort_assignment)
    session = Session()
    if CohortAssignment.__tablename__ in engine.table_names():
        session.execute(DropTable(CohortAssignment.__table__))
    session.execute(CreateTable(CohortAssignment.__table__))
    session.add_all(cohort_assignments)
    session.commit()
    session.close()

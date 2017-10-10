from feather import read_dataframe
from sqlalchemy.orm import sessionmaker
from sqlalchemy.schema import CreateTable, DropTable
import pandas as pd
from models import Sale

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
    sales_df = read_dataframe('feathers/sales.feather')
    sales_dicts = sales_df.to_dict(orient='record')
    sales = []
    for record in sales_dicts:
        sale = Sale(
            factory_fault = record.get('factory_fault'),
            hidden = record.get('hidden'),
            is_shipped = record.get('is_shipped'),
            item_returned = record.get('item_returned'),
            repeat_purchase = record.get('repeat_purchase'),
            return_requested = record.get('return_requested'),
            acquisition_date = convert_date(record.get('acquisition_date'), 'Date'),
            correct_ship_date = convert_date(record.get('correct_ship_date'), 'Date'),
            estimated_ship_date = convert_date(record.get('estimated_ship_date'), 'Date'),
            li_ship_date = convert_date(record.get('li_ship_date'), 'Date'),
            o_ship_date = convert_date(record.get('o_ship_date'), 'Date'),
            order_date = convert_date(record.get('order_date'), 'Date'),
            ship_date = convert_date(record.get('ship_date'), 'Date'),
            available_on = convert_date(record.get('available_on'), 'DateTime'),
            completed_timestamp = convert_date(record.get('completed_timestamp'), 'DateTime'),
            refunded_at = convert_date(record.get('refunded_at'), 'DateTime'),
            requested_at = convert_date(record.get('requested_at'), 'DateTime'),
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
            o_adjustments = record.get('o_adjustments'),
            o_other_adjustments = record.get('o_other_adjustments'),
            o_promotions = record.get('o_promotions'),
            o_shipping = record.get('o_shipping'),
            o_taxes = record.get('o_taxes'),
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
            product_live = record.get('product_live'),
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
    return sales[0:9]
